#'
#' @import doParallel
#' @import parallel
#'
#' @importFrom foreach foreach
#' @importFrom gdalUtils gdal_translate gdalwarp gdalbuildvrt mosaic_rasters
#' @importFrom logging loginfo
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
  dir.create(out_dir)
  logging::loginfo(paste0("Output directory set: ", out_dir))

  # set up parallel processing based on number of available or specified cores
  if (is.na(cores)) cores <- parallel::detectCores() -1
  par_cluster <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(par_cluster)
  logging::loginfo(paste0("Parallel processing initialized on ", cores, " cores"))
  logging::loginfo("--------------------------------------------------------------")

  # list all hdf files in in_dir and make sure they are MODIS
  hdf_files <- list.files(in_dir, pattern = ".*M(O|Y)D.*.hdf", full.names = TRUE, no.. = TRUE)
  if(length(hdf_files) < 1) stop("No MODIS .hdf files found in input directory.")

  # save aoi to a tempfile for later AOI masking using gdalwarp
  temp_aoi <- file.path(tempdir(), "aoi_mask.shp")
  sf::st_write(aoi, temp_aoi, overwrite = TRUE, delete_dsn = TRUE, quiet = TRUE)

  # ---- SDS Extraction ----
  logging::loginfo("Extracting subdatasets from .hdf files...")
  foreach::foreach(f = 1:length(hdf_files), .packages = c("raster", "gdalUtils", "MODIS")) %dopar% {
    out_file <- file.path(out_dir, paste0(strsplit(basename(hdf_files[f]), ".hdf")[[1]][1], "_extract.tif"))
    sds <- MODIS::getSds(hdf_files[f])[[2]]

    temp_env_vi <- raster::rasterTmpFile()
    temp_env_doy <- raster::rasterTmpFile()
    temp_env_qa <- raster::rasterTmpFile()

    raster::extension(temp_env_vi) <- ".tif"
    raster::extension(temp_env_doy) <- ".tif"
    raster::extension(temp_env_qa) <- ".tif"

    gdalUtils::gdal_translate(grep(vi, sds, value = TRUE), dst_dataset = temp_env_vi)
    gdalUtils::gdal_translate(grep("day of the year", sds, value = TRUE), dst_dataset = temp_env_doy)
    gdalUtils::gdal_translate(grep("pixel reliability", sds, value = TRUE), dst_dataset = temp_env_qa)

    r_vi <- raster::raster(temp_env_vi) / 10000 # TODO: check value range for correct rescaling
    r_doy <- raster::raster(temp_env_doy)
    r_qa <- raster::raster(temp_env_qa)

    r_stack <- raster::stack(list(r_vi, r_doy, r_qa))
    names(r_stack) <- c(vi, "DOY", "QA")

    writeRaster(r_stack, filename = out_file, datatype = 'INT2U', format = 'GTiff', overwrite = TRUE)
  }
  raster::removeTmpFiles(h=0)

  # ---- Tile Mosaicing ----
  logging::loginfo("Mosaicing tiles...")
  img_files <- list.files(out_dir, '.*M(O|Y)D.*_extract.tif', full.names = TRUE, no.. = TRUE)

  if (length(.get_unique_tilestr(img_files)) > 1) {
    dates <- unique(do.call("c", lapply(img_files, .getMODIS_date)))

    foreach::foreach(f = 1:length(dates), .packages = "gdalUtils") %dopar% {
      date_curr_str <- strftime(dates[f], format = '%Y%j')
      date_curr_files <- img_files[grepl(paste0("\\.A", date_curr_str, "\\."), img_files)]
      out_file <- file.path(out_dir, paste0(date_curr_str, "_mosaic.tif"))
      gdalUtils::mosaic_rasters(date_curr_files, out_file, of = 'GTiff', ot = 'UInt16')
    }
  } else {
    # just rename to pretend mosaicing was done
    logging::loginfo("Skipped mosaicing since only one unique tile found.")
    for (i in 1:length(img_files)) {
      date_str <- .getMODIS_datestr(.getMODIS_date(basename(img_files[i])))
      out_file <- file.path(out_dir, paste0(date_str, "_mosaic.tif"))
      file.rename(from = img_files[i], to = out_file)
    }
  }

  # cleanup
  do.call(file.remove, list(list.files(out_dir, paste0(".*M(O|Y)D.*_extract.tif"), full.names = TRUE, no.. = TRUE)))

  # ---- Image Reprojection ----

  if (!is.na(out_proj)) {
    img_files <- list.files(out_dir, paste0('_mosaic.tif'), full.names = TRUE, no.. = TRUE)

    logging::loginfo(paste0("Reprojecting images to: ", out_proj))
    foreach::foreach(f = 1:length(img_files), .packages = 'gdalUtils') %dopar% {
      out_file <- file.path(out_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_proj.tif"))
      gdalUtils::gdalwarp(img_files[f], out_file, t_srs = out_proj, of = 'GTiff', ot = 'UInt16')
    }

    # cleanup
    do.call(file.remove, list(list.files(out_dir, paste0("_mosaic.tif"), full.names = TRUE, no.. = TRUE)))
  } else {
    logging::loginfo("Skipped image reprojection since no out_proj specified.")
  }

  # ---- AOI Cropping ----

  if (!is.na(out_proj)) {
    img_files <- list.files(out_dir, paste0('_proj.tif'), full.names = TRUE, no.. = TRUE)
  } else {
    img_files <- list.files(out_dir, paste0('_mosaic.tif'), full.names = TRUE, no.. = TRUE)
  }
  aoi_ext <- try(raster::extent(aoi))
  if (class(aoi_ext) == 'try-error') stop(paste0("Unable to retrieve extent from aoi: ", as.character(aoi_ext[1])))

  logging::loginfo("Cropping images to AOI...")
  foreach::foreach(f = 1:length(img_files), .packages = c('gdalUtils', 'raster')) %dopar% {
    out_file <- file.path(out_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_crop.tif"))
    temp_env <- tempfile(fileext = ".vrt")
    catch <- gdalUtils::gdalbuildvrt(img_files[f], temp_env, te = c(aoi_ext@xmin, aoi_ext@ymin, aoi_ext@xmax, aoi_ext@ymax))
    x <- raster::stack(temp_env)
    raster::writeRaster(x, out_file, format = 'GTiff', datatype = 'INT2U')
  }

  # cleanup
  if (!is.na(out_proj)) {
    do.call(file.remove, list(list.files(out_dir, paste0("_proj.tif"), full.names = TRUE, no.. = TRUE)))
  } else {
    do.call(file.remove, list(list.files(out_dir, paste0("_mosaic.tif"), full.names = TRUE, no.. = TRUE)))
  }

  # ---- AOI Masking ----

  img_files <- list.files(out_dir, paste0('_crop.tif'), full.names = TRUE, no.. = TRUE)

  logging::loginfo("Masking images to AOI...")
  foreach::foreach(f = 1:length(img_files), .packages = 'gdalUtils') %dopar% {
    out_file <- file.path(out_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_prep.tif"))
    gdalUtils::gdalwarp(img_files[f], out_file, cutline = temp_aoi, of = 'GTiff', ot = 'Uint16')
  }

  # cleanup
  do.call(file.remove, list(list.files(out_dir, "_crop.tif", full.names = TRUE, no.. = TRUE)))


  # stop the multicore processing cluster
  parallel::stopCluster(par_cluster)
  logging::loginfo("--------------------------------------------------------------")
  logging::loginfo("Parallel processing cluster stopped.")

  logging::loginfo("Preprocessing successful.")
}

#prepMODIS(hdf_dir, prep_dir, aoi, c('NDVI', 'EVI'), out_proj = "EPSG:32632")
