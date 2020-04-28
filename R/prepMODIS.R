#'
#' @title Prepare donwloaded MODIS archives for fitting.
#'
#' @import doSNOW
#' @import parallel
#'
#' @importFrom foreach foreach
#' @importFrom gdalUtils gdal_translate gdalwarp gdalbuildvrt mosaic_rasters
#' @importFrom logging loginfo logdebug
#' @importFrom MODIS getSds
#' @importFrom raster raster stack rasterTmpFile removeTmpFiles extension extent writeRaster
#'
#' @export
#'
prepMODIS <- function(in_dir, out_dir, aoi, vi='NDVI', out_proj=NA, cores=NA) {

  # check if input parameters are valid on the first look
  valid_VIs <- c('NDVI', 'EVI')
  if (!(all(vi %in% valid_VIs))) stop("Specified vi is not supported.")
  if (!dir.exists(in_dir)) stop("Input directory not found.")
  if (!inherits(aoi, 'sf')) stop("AOI must be of type sf.")
  if (!is.na(out_proj)) {
    if (!(any(grepl('EPSG:', out_proj)) | any(grepl('+proj=', out_proj)) | any(grepl('.prf', out_proj)))) {
      stop("Output Projection is not valid.")
    }
  }

  logging::loginfo("Starting preprocessing...")

  # setup data structure
  if (!dir.exists(out_dir)) dir.create(out_dir) else stop("Output directory already exists.")
  logging::loginfo(paste0("Output directory set: ", out_dir))
  orig_dir <- file.path(out_dir, "orig")
  dir.create(orig_dir)
  logging::loginfo(paste0("Original images directory set: ", orig_dir))
  sds <- c(vi, "DOY", "QA")

  # set up parallel processing based on number of available or specified cores
  if (is.na(cores)) cores <- parallel::detectCores() -1
  c1 <- parallel::makePSOCKcluster(cores)
  doSNOW::registerDoSNOW(c1)
  logging::loginfo(paste0("Parallel processing initialized on ", cores, " cores"))

  # list all hdf files in in_dir and make sure they are MODIS
  hdf_files <- list.files(in_dir, pattern = ".*M(O|Y)D.*.hdf", full.names = TRUE, no.. = TRUE)
  if(length(hdf_files) < 1) stop("No MODIS .hdf files found in input directory.")

  # save aoi to a tempfile for later AOI masking using gdalwarp
  temp_aoi <- file.path(tempdir(), "aoi_mask.shp")
  sf::st_write(aoi, temp_aoi, overwrite = TRUE, delete_dsn = TRUE, quiet = TRUE)

  # ---- SDS Extraction ----

  logging::loginfo("Extracting subdatasets from .hdf files...")

  # Setup progress bar
  pb <- txtProgressBar(min = 1, max = length(hdf_files), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts = list(progress=progress)

  foreach::foreach(f = 1:length(hdf_files), .packages = c("raster", "gdalUtils", "MODIS"), .options.snow = opts) %dopar% {
    out_vi_file  <- file.path(orig_dir, paste0(strsplit(basename(hdf_files[f]), ".hdf")[[1]][1], "_", vi, "_extract.tif"))
    out_doy_file <- file.path(orig_dir, paste0(strsplit(basename(hdf_files[f]), ".hdf")[[1]][1], "_DOY_extract.tif"))
    out_qa_file  <- file.path(orig_dir, paste0(strsplit(basename(hdf_files[f]), ".hdf")[[1]][1], "_QA_extract.tif"))

    sds <- MODIS::getSds(hdf_files[f])[[2]]

    temp_env_vi  <- raster::rasterTmpFile()
    temp_env_doy <- raster::rasterTmpFile()
    temp_env_qa  <- raster::rasterTmpFile()

    raster::extension(temp_env_vi) <- ".tif"
    raster::extension(temp_env_doy) <- ".tif"
    raster::extension(temp_env_qa) <- ".tif"

    gdalUtils::gdal_translate(grep(vi, sds, value = TRUE), dst_dataset = temp_env_vi)
    gdalUtils::gdal_translate(grep("day of the year", sds, value = TRUE), dst_dataset = temp_env_doy)
    gdalUtils::gdal_translate(grep("pixel reliability", sds, value = TRUE), dst_dataset = temp_env_qa)

    r_vi  <- raster::raster(temp_env_vi) / 10000 # TODO: check value range for correct rescaling
    r_doy <- raster::raster(temp_env_doy)
    r_qa  <- raster::raster(temp_env_qa)

    names(r_vi)  <- vi
    names(r_doy) <- "DOY"
    names(r_qa)  <- "QA"

    writeRaster(r_vi, filename = out_vi_file, datatype = 'INT2U', format = 'GTiff', overwrite = TRUE)
    writeRaster(r_doy, filename = out_doy_file, datatype = 'INT2U', format = 'GTiff', overwrite = TRUE)
    writeRaster(r_qa, filename = out_qa_file, datatype = 'INT2U', format = 'GTiff', overwrite = TRUE)
  }
  raster::removeTmpFiles(h=0)
  close(pb)

  # ---- Tile Mosaicing ----

  logging::loginfo("Mosaicing tiles...")

  for (i in 1:length(sds)) {
    img_files <- list.files(orig_dir, paste0('.*M(O|Y)D.*', sds[i], '_extract.tif'), full.names = TRUE, no.. = TRUE)

    logging::logdebug(paste0("Processing ", sds[i], " subdatasets..."))
    if (length(.get_unique_tilestr(img_files)) > 1) {
      dates <- unique(do.call("c", lapply(img_files, .getMODIS_date)))

      # Setup progress bar
      pb <- txtProgressBar(min = 1, max = length(dates), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts = list(progress=progress)

      foreach::foreach(f = 1:length(dates), .packages = "gdalUtils", .options.snow = opts) %dopar% {
        date_curr_str <- strftime(dates[f], format = '%Y%j')
        date_curr_files <- img_files[grepl(paste0("\\.A", date_curr_str, "\\."), img_files)]
        out_file <- file.path(orig_dir, paste0(date_curr_str, "_", sds[i], "_mosaic.tif"))
        gdalUtils::mosaic_rasters(date_curr_files, out_file, of = 'GTiff', ot = 'UInt16')
      }
      close(pb)
    } else {
      # just rename to pretend mosaicing was done
      logging::loginfo("Skipped mosaicing since only one unique tile found.")
      for (i in 1:length(img_files)) {
        date_str <- .getMODIS_datestr(.getMODIS_date(basename(img_files[i])))
        out_file <- file.path(orig_dir, paste0(date_str, "_", vi, "_mosaic.tif"))
        file.rename(from = img_files[i], to = out_file)
      }
    }
  }

  # cleanup
  do.call(file.remove, list(list.files(orig_dir, ".*M(O|Y)D.*_extract.tif", full.names = TRUE, no.. = TRUE)))

  # ---- Image Reprojection ----

  if (!is.na(out_proj)) {
    logging::loginfo(paste0("Reprojecting images to: ", out_proj))

    for (i in 1:length(sds)) {
      logging::logdebug(paste0("Processing ", sds[i], " subdatasets..."))
      img_files <- list.files(orig_dir, paste0('_', sds[i], '_mosaic.tif'), full.names = TRUE, no.. = TRUE)

      # Setup progress bar
      pb <- txtProgressBar(min = 1, max = length(img_files), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts = list(progress=progress)

      foreach::foreach(f = 1:length(img_files), .packages = 'gdalUtils', .options.snow = opts) %dopar% {
        out_file <- file.path(orig_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_", sds[i], "_proj.tif"))
        gdalUtils::gdalwarp(img_files[f], out_file, t_srs = out_proj, of = 'GTiff', ot = 'UInt16')
      }

      close(pb)
    }

    # cleanup
    do.call(file.remove, list(list.files(orig_dir, "_mosaic.tif", full.names = TRUE, no.. = TRUE)))
  } else {
    logging::loginfo("Skipped image reprojection since no out_proj specified.")
  }

  # ---- AOI Cropping ----

  logging::loginfo("Cropping images to AOI...")

  for (i in 1:length(sds)) {
    logging::logdebug(paste0("Processing ", sds[i], " subdatasets..."))

    if (!is.na(out_proj)) {
      img_files <- list.files(orig_dir, paste0('_', sds[i], '_proj.tif'), full.names = TRUE, no.. = TRUE)
    } else {
      img_files <- list.files(orig_dir, paste0('_', sds[i], '_mosaic.tif'), full.names = TRUE, no.. = TRUE)
    }
    aoi_ext <- try(raster::extent(aoi))
    if (class(aoi_ext) == 'try-error') stop(paste0("Unable to retrieve extent from aoi: ", as.character(aoi_ext[1])))

    # Setup progress bar
    pb <- txtProgressBar(min = 1, max = length(img_files), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts = list(progress=progress)

    foreach::foreach(f = 1:length(img_files), .packages = c('gdalUtils', 'raster'), .options.snow = opts) %dopar% {
      out_file <- file.path(orig_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_", sds[i], "_crop.tif"))
      temp_env <- tempfile(fileext = ".vrt")
      catch <- gdalUtils::gdalbuildvrt(img_files[f], temp_env, te = c(aoi_ext@xmin, aoi_ext@ymin, aoi_ext@xmax, aoi_ext@ymax))
      x <- raster::stack(temp_env)
      raster::writeRaster(x, out_file, format = 'GTiff', datatype = 'INT2U')
    }

    close(pb)
  }

  # cleanup
  if (!is.na(out_proj)) {
    do.call(file.remove, list(list.files(orig_dir, "_proj.tif", full.names = TRUE, no.. = TRUE)))
  } else {
    do.call(file.remove, list(list.files(orig_dir, "_mosaic.tif", full.names = TRUE, no.. = TRUE)))
  }

  # ---- AOI Masking ----

  logging::loginfo("Masking images to AOI...")

  for (i in 1:length(sds)) {
    logging::logdebug(paste0("Processing ", sds[i], " subdatasets..."))
    img_files <- list.files(orig_dir, paste0('_', sds[i], '_crop.tif'), full.names = TRUE, no.. = TRUE)

    # Setup progress bar
    pb <- txtProgressBar(min = 1, max = length(img_files), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts = list(progress=progress)

    foreach::foreach(f = 1:length(img_files), .packages = 'gdalUtils', .options.snow = opts) %dopar% {
      out_file <- file.path(orig_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_", sds[i], "_prep.tif"))
      gdalUtils::gdalwarp(img_files[f], out_file, cutline = temp_aoi, of = 'GTiff', ot = 'Uint16')
    }

    close(pb)
  }


  # cleanup
  do.call(file.remove, list(list.files(orig_dir, "_crop.tif", full.names = TRUE, no.. = TRUE)))

  # ---- Flat Binary Conversion ----

  logging::loginfo("Converting prepared images to flat binary files...")

  img_files <- list.files(orig_dir, "_prep.tif", full.names = TRUE, no.. = TRUE)

  # Setup progress bar
  pb <- txtProgressBar(min = 1, max = length(img_files), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts = list(progress=progress)

  foreach::foreach(f = 1:length(img_files), .packages = "gdalUtils", .options.snow = opts) %dopar% {
    out_file <- file.path(out_dir, paste0(strsplit(basename(img_files[f]), '_')[[1]][1],
                                          "_", strsplit(basename(img_files[f]), '_')[[1]][2],
                                          "_prepbin.bin"))
    gdalUtils::gdal_translate(img_files[f], out_file, of = 'ENVI', ot = 'UInt16', co = "INTERLEAVE=BIL")
  }

  close(pb)

  # stop the multicore processing cluster
  parallel::stopCluster(c1)
  logging::loginfo("Parallel processing cluster stopped.")

  logging::loginfo("Preprocessing successful.")
}

#prepMODIS(hdf_dir, prep_dir, aoi, c('NDVI', 'EVI'), out_proj = "EPSG:32632")
