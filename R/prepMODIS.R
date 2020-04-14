prepMODIS <- function(in_dir, out_dir, aoi, vi='NDVI', out_proj=NA, cores=NA) {

  # check if input parameters are valied on the first look
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

  # setup data structure:
  # out_dir
  # |- [NDVI]
  # |- [EVI]
  # |- QA
  # |- DOY
  dir.create(out_dir)
  if ('NDVI' %in% vi) {
    ndvi_dir <- file.path(out_dir, 'NDVI')
    dir.create(ndvi_dir)
  }
  if ('EVI' %in% vi) {
    evi_dir <- file.path(out_dir, 'EVI')
    dir.create(evi_dir)
  }
  qa_dir <- file.path(out_dir, 'QA')
  doy_dir <- file.path(out_dir, 'DOY')
  dir.create(qa_dir)
  dir.create(doy_dir)
  logging::loginfo(paste0("Output directory set: ", out_dir))

  # Get all subdataset names that should be processed based on the created directories
  sds <- list.dirs(out_dir, full.names = FALSE, recursive = FALSE)

  # set up parallel processing based on number of available or specified cores
  if (is.na(cores)) cores <- parallel::detectCores() -1
  par_cluster <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(par_cluster)
  logging::loginfo(paste0("Parallel processing initialized on ", cores, " cores"))

  # list all hdf files in in_dir and make sure they are MODIS
  hdf_files <- list.files(in_dir, pattern = ".*M(O|Y)D.*.hdf", full.names = TRUE, no.. = TRUE)
  if(length(hdf_files) < 1) stop("No MODIS .hdf files found in input directory.")

  # save aoi to a tempfile for later AOI masking using gdalwarp
  temp_aoi <- file.path(tempdir(), "aoi_mask.shp")
  sf::st_write(aoi, temp_aoi, overwrite = TRUE, delete_dsn = TRUE, quiet = TRUE)

  # Loop trough all sds and execute preprocessing steps
  for (i in 1:length(sds)) {
    sd <- sds[i]
    sd_out_dir <- file.path(out_dir, sd)
    sd_idx <- .get_sd_idx(hdf_files[1], sd)
    out_ot <- 'Int16'
    logging::loginfo(paste0("Starting processing of subdataset ", i, " of ", length(sds), ": ", sd))

    # ---- SDS Extraction ----
    logging::logdebug("Extracting subdataset from .hdf archives...")
    foreach::foreach(f = 1:length(hdf_files), .packages = "gdalUtils") %dopar% {
      out_file <- file.path(sd_out_dir, paste0(strsplit(basename(hdf_files[f]), ".hdf")[[1]][1], "_", sd, ".tif"))
      gdalUtils::gdal_translate(hdf_files[f], out_file, sd_index = sd_idx, of = 'GTiff', ot = out_ot)
    }

    # ---- Tile Mosaicing ----

    img_files <- list.files(sd_out_dir, paste0(".*M(O|Y)D.*_", sd, ".tif"), full.names = TRUE, no.. = TRUE)
    if (length(.get_unique_tilestr(img_files)) > 1) {
      dates <- unique(do.call("c", lapply(img_files, .getMODIS_date)))

      logging::logdebug("Mosaicing MODIS tiles...")
      foreach::foreach(f = 1:length(dates), .packages = "gdalUtils") %dopar% {
        date_curr_str <- strftime(dates[f], format = '%Y%j')
        date_curr_files <- img_files[grepl(paste0("\\.A", date_curr_str, "\\."), img_files)]
        product_name <- strsplit(basename(date_curr_files[1]), "\\.")[[1]][1]
        out_file <- file.path(sd_out_dir, paste0(product_name, ".", date_curr_str, "_", sd, "_", "mosaic.tif"))
        gdalUtils::mosaic_rasters(date_curr_files, out_file, of = 'GTiff', ot = out_ot)
      }
    } else { # just rename to pretend mosaicing was done
      logging::logdebug("Skipping mosaicing since only one unique tile found.")
      for (j in 1:length(img_files)) {
        product_name <- strsplit(basename(img_files[j]), '\\.')[[1]][1]
        date_str <- .getMODIS_datestr(.getMODIS_date(basename(img_files[j])))
        out_file <- file.path(sd_out_dir, paste0(product_name, ".", date_str, "_", sd, "_", "mosaic.tif"))
        file.rename(from = img_files[j], to = out_file)
      }
    }

    # cleanup
    do.call(file.remove, list(list.files(sd_out_dir, paste0(".*M(O|Y)D.*_", sd, ".tif"), full.names = TRUE, no.. = TRUE)))

    # ---- Image Reprojection ----

    if (!is.na(out_proj)) {
      img_files <- list.files(sd_out_dir, paste0('.*M(O|Y)D.*_', sd, '_mosaic.tif'), full.names = TRUE, no.. = TRUE)

      logging::logdebug(paste0("Reprojecting images to: ", out_proj))
      foreach::foreach(f = 1:length(img_files), .packages = 'gdalUtils') %dopar% {
        out_file <- file.path(sd_out_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_", sd, "_proj.tif"))
        gdalUtils::gdalwarp(img_files[f], out_file, t_srs = out_proj, of = 'GTiff')
      }

      # cleanup
      do.call(file.remove, list(list.files(sd_out_dir, paste0(".*M(O|Y)D.*", "_mosaic.tif"), full.names = TRUE, no.. = TRUE)))
    }

    # ---- AOI Cropping ----

    if (!is.na(out_proj)) {
      img_files <- list.files(sd_out_dir, paste0('.*M(O|Y)D.*_', sd, '_proj.tif'), full.names = TRUE, no.. = TRUE)
    } else {
      img_files <- list.files(sd_out_dir, paste0('.*M(O|Y)D.*_', sd, '_mosaic.tif'), full.names = TRUE, no.. = TRUE)
    }
    aoi_ext <- try(raster::extent(aoi))
    if (class(aoi_ext) == 'try-error') stop(paste0("Unable to retrieve extent from aoi: ", as.character(aoi_ext[1])))

    logging::logdebug("Cropping images to AOI...")
    foreach::foreach(f = 1:length(img_files), .packages = c('gdalUtils', 'raster')) %dopar% {
      out_file <- file.path(sd_out_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_", sd, "_crop.tif"))
      temp_env <- tempfile(fileext = ".vrt")
      catch <- gdalUtils::gdalbuildvrt(img_files[f], temp_env, te = c(aoi_ext@xmin, aoi_ext@ymin, aoi_ext@xmax, aoi_ext@ymax))
      x <- raster::stack(temp_env)
      raster::writeRaster(x, out_file, format = 'GTiff')
    }

    # cleanup
    if (!is.na(out_proj)) {
      do.call(file.remove, list(list.files(sd_out_dir, paste0(".*M(O|Y)D.*", "_proj.tif"), full.names = TRUE, no.. = TRUE)))
    } else {
      do.call(file.remove, list(list.files(sd_out_dir, paste0(".*M(O|Y)D.*", "_mosaic.tif"), full.names = TRUE, no.. = TRUE)))
    }

    # ---- AOI Masking ----

    img_files <- list.files(sd_out_dir, paste0('.*M(O|Y)D.*_', sd, '_crop.tif'), full.names = TRUE, no.. = TRUE)

    logging::logdebug("Masking images to AOI...")
    foreach::foreach(f = 1:length(img_files), .packages = 'gdalUtils') %dopar% {
      out_file <- file.path(sd_out_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_", sd, "_mask.tif"))
      gdalUtils::gdalwarp(img_files[f], out_file, cutline = temp_aoi, of = 'GTiff')
    }

    # cleanup
    do.call(file.remove, list(list.files(sd_out_dir, paste0(".*M(O|Y)D.*", "_crop.tif"), full.names = TRUE, no.. = TRUE)))
  }

  # ---- Pixel Reliability Masking ----

  logging::loginfo("Starting Layer corrections...")
  qa_files <- list.files(qa_dir, pattern = '.*M(O|Y)D.*_QA_mask.tif', full.names = TRUE, no.. = TRUE)

  # loop trough all extracted VIs an DOY
  ds <- c(vi, "DOY")
  for (i in 1:length(ds)) {
    img_files <- list.files(file.path(out_dir, ds[i]), pattern=paste0('.*M(O|Y)D.*_', ds[i], '_mask.tif'),
                            full.names = TRUE, no.. = TRUE)

    logging::logdebug(paste0("Masking images based on pixel reliability for ", ds[i], "..."))
    foreach::foreach(f = 1:length(qa_files), .packages = 'raster') %dopar% {
      out_file <- file.path(out_dir, ds[i], paste0(strsplit(basename(qa_files[f]), "_")[[1]][1], "_", ds[i], "_qmask.tif"))
      img <- grep(strsplit(basename(qa_files[f]), '_')[[1]][1], img_files, value = TRUE)[1]
      r <- raster::raster(img)
      qa <- raster::raster(qa_files[f])

      # all pixels with QA flag 2 = Snow/Ice and 3 = Cloudy will be set to NA
      r[qa == 2] <- raster::NAvalue(r)
      r[qa == 3] <- raster::NAvalue(r)

      raster::writeRaster(r, out_file, format = 'GTiff')
    }

    # cleanup
    do.call(file.remove, list(list.files(file.path(out_dir, ds[i]), paste0(".*M(O|Y)D.*", "_mask.tif"),
                                         full.names = TRUE, no.. = TRUE)))
  }

  # ---- VI Rescaling ----

  # loop through all extracted VIs
  for (i in 1:length(vi)) {

    img_files <- list.files(file.path(out_dir, vi[i]), pattern=paste0('.*M(O|Y)D.*_', vi[i], '_qmask.tif'),
                            full.names = TRUE, no.. = TRUE)
    logging::logdebug(paste0("Rescaling VI for: ", vi[i]))
    foreach::foreach(f = 1:length(img_files), .packages = 'raster') %dopar% {
      out_file <- file.path(out_dir, vi[i], paste0(strsplit(basename(qa_files[f]), "_")[[1]][1], "_", ds[i], "_prep.tif"))
      r <- raster::raster(img_files[f])
      values(r) <- values(r) * 0.0001
      raster::writeRaster(r, out_file, format = 'GTiff')
    }

    # cleanup
    do.call(file.remove, list(list.files(file.path(out_dir, vi[i]), paste0(".*M(O|Y)D.*", "_qmask.tif"),
                                         full.names = TRUE, no.. = TRUE)))
  }

  # stop the multicore processing cluster
  parallel::stopCluster(par_cluster)
  logging::loginfo("Parallel processing cluster stopped.")

  logging::loginfo("Preprocessing successful.")
}

#prepMODIS(hdf_dir, prep_dir, aoi, c('NDVI', 'EVI'), out_proj = "EPSG:32632")
