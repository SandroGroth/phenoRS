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

  # Get all subdataset names that should be processed based on the created directories
  sds <- list.dirs(out_dir, full.names = FALSE, recursive = FALSE)

  # set up parallel processing based on number of available or specified cores
  if (is.na(cores)) cores <- parallel::detectCores() -1
  par_cluster <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(par_cluster)

  # list all hdf files in in_dir and make sure they are MODIS
  hdf_files <- list.files(in_dir, pattern = ".*M(O|Y)D.*.hdf", full.names = TRUE, no.. = TRUE)
  if(length(hdf_files) < 1) stop("No MODIS .hdf files found in input directory.")

  # save aoi to a tempfile for later AOI masking using gdalwarp
  temp_aoi <- file.path(tempdir(), "aoi_mask.shp")
  sf::st_write(aoi, temp_aoi, overwrite = TRUE, delete_dsn = TRUE)

  # Loop trough all sds and execute preprocessing steps
  for (i in 1:length(sds)) {
    sd <- sds[i]
    sd_out_dir <- file.path(out_dir, sd)
    sd_idx <- .get_sd_idx(hdf_files[1], sd)
    # TODO: Check necessary -ot flag for each sd

    # ---- SDS Extraction ----

    foreach::foreach(f = 1:length(hdf_files), .packages = "gdalUtils") %dopar% {
      out_file <- file.path(sd_out_dir, paste0(strsplit(basename(hdf_files[f]), ".hdf")[[1]][1], "_", sd, ".tif"))
      gdalUtils::gdal_translate(hdf_files[f], out_file, sd_index = sd_idx, of = 'GTiff', ot = "int16")
    }

    # ---- Tile Mosaicing ----
    # TODO: Skip Mosaicing when only one tile is found -> get_unique_tiles()

    img_files <- list.files(sd_out_dir, paste0(".*M(O|Y)D.*_", sd, ".tif"), full.names = TRUE, no.. = TRUE)
    dates <- unique(do.call("c", lapply(img_files, .getMODIS_date)))

    foreach::foreach(f = 1:length(dates), .packages = "gdalUtils") %dopar% {
      date_curr_str <- strftime(dates[f], format = '%Y%j')
      date_curr_files <- img_files[grepl(paste0("\\.A", date_curr_str, "\\."), img_files)]
      product_name <- strsplit(basename(date_curr_files[1]), "\\.")[[1]][1]
      out_file <- file.path(sd_out_dir, paste0(product_name, ".", date_curr_str, "_", sd, "_", "mosaic.tif"))
      gdalUtils::mosaic_rasters(date_curr_files, out_file, of = 'GTiff', ot = 'Int16')
    }

    # cleanup
    do.call(file.remove, list(list.files(sd_out_dir, paste0(".*M(O|Y)D.*_", sd, ".tif"), full.names = TRUE, no.. = TRUE)))

    # ---- Image Reprojection ----

    if (!is.na(out_proj)) {
      img_files <- list.files(sd_out_dir, paste0('.*M(O|Y)D.*_', sd, '_mosaic.tif'), full.names = TRUE, no.. = TRUE)

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

    foreach::foreach(f = 1:length(img_files), .packages = 'gdalUtils') %dopar% {
      out_file <- file.path(sd_out_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_", sd, "_mask.tif"))
      gdalUtils::gdalwarp(img_files[f], out_file, cutline = temp_aoi, of = 'GTiff')
    }

    # cleanup
    do.call(file.remove, list(list.files(sd_out_dir, paste0(".*M(O|Y)D.*", "_crop.tif"), full.names = TRUE, no.. = TRUE)))
  }

  # stop the multicore processing cluster
  parallel::stopCluster(par_cluster)
}

#prepMODIS(hdf_dir, prep_dir, aoi, c('NDVI', 'EVI'), out_proj = "EPSG:32632")
