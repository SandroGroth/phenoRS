.extract_sd <- function(in_dir, out_dir, band_id, cores=NA, check_existing=T) {

  # check if output directory exists, otherwise create it
  if(!dir.exists(out_dir)) try(dir.create(out_dir))
  logging::logdebug(paste0("Subdataset directory set: ", out_dir))

  # list all MODIS hdf files in input diretory
  in_files <- list.files(in_dir, ".*M(O|Y)D.*.hdf", full.names = T, no.. = T)

  # Get Product information in order to extract the right subdataset
  prod_name <- strsplit(basename(in_files[1]), '\\.')[[1]][1] # e.g. MOD13Q1
  band_nr <- .get_sdnr(prod_name, band_id)

  # setup parallel processing
  if (is.na(cores)) cores <- parallel::detectCores() - 1
  c1 <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(c1)
  logging::logdebug(paste0("Set up parallel processing on ", cores, " cores"))

  # execute parallelized extraction
  logging::logdebug(paste0("Extracting ", band_id, " of ", length(in_files), " files to "), out_dir, " ...")
  foreach::foreach(f = 1:length(in_files), .packages = "gdalUtils") %dopar% {
    out_file <- file.path(out_dir, paste0(strsplit(basename(in_files[f]), ".hdf")[[1]][1], ".", band_id, ".tif"))
    if (isTRUE(check_existing)) {
      if (!file.exists(out_file)) gdalUtils::gdal_translate(in_files[f], out_file, sd_index = band_nr)
    } else gdalUtils::gdal_translate(in_files[f], out_file, sd_index = band_nr)
  }

  parallel::stopCluster(c1)
}

.mosaic_tiles <- function(in_dir, out_dir, band_id, cores=NA, check.existing=T) {

  # check if output directory exists, otherwise create it
  if(!dir.exists(out_dir)) try(dir.create(out_dir))
  logging::logdebug(paste0("Subdataset directory set: ", out_dir))

  # get all available dates from files
  mod_files <- list.files(in_dir, ".*M(O|Y)D.*.tif", full.names = T, no.. = T)
  dates <- unique(do.call("c", lapply(mod_files, .getMODIS_date)))

  # setup parallel processing
  if (is.na(cores)) cores <- parallel::detectCores() - 1
  c1 <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(c1)
  logging::logdebug(paste0("Set up parallel processing on ", cores, " cores"))

  logging::logdebug(paste0("Mosaicing ", band_id, " of ", length(mod_files), " files to "), out_dir, " ...")
  foreach::foreach(f = 1:length(dates), .packages = "gdalUtils") %dopar% {
    date_cur_str <- strftime(dates[f], format = '%Y%j')
    files_currdate <- list.files(in_dir, pattern = paste0(".*M(O|Y)D.*", date_cur_str , ".*", band_id, ".tif"),
                                 full.names = T, no.. = T)

    prod_name <- strsplit(basename(files_currdate[1]), "\\.")[[1]][1]
    out_file <- file.path(out_dir, paste0(prod_name, ".", date_cur_str, ".", band_id, ".mosaic.tif"))

    gdalUtils::mosaic_rasters(files_currdate, out_file, of = "GTiff", verbose=TRUE)
  }
  stopCluster(c1)
}

#'
#' @source \link[getSpatialData]{cropFAST}
#'
.crop <- function(in_dir, out_dir, aoi, cores=NA) {

  # check if output directory exists, otherwise create it
  if (!dir.exists(out_dir)) try(dir.create(out_dir))
  logging::logdebug(paste0("Cropped image directory set: ", out_dir))

  # get all files in input directory
  mod_files <- list.files(in_dir, ".*M(O|Y)D.*.tif", full.names = T, no.. = T)

  # try getting extent of aoi
  if (!inherits(aoi, "sf")) stop("AOI must be of type sf.")
  ext <- try(raster::extent(aoi))

  # setup parallel processing
  if (is.na(cores)) cores <- parallel::detectCores() - 1
  c1 <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(c1)
  logging::logdebug(paste0("Set up parallel processing on ", cores, " cores"))

  # execute parallel cropping
  logging::logdebug("Cropping ", length(mod_files), " using specified extent to ", out_dir)
  foreach::foreach(f = 1:length(mod_files), .packages = c("gdalUtils", "raster")) %dopar% {
    out_file <- file.path(out_dir, paste0(strsplit(basename(mod_files[f]), ".tif")[[1]][1], ".crop.tif"))
    temp.env <- paste0(paste0(head(strsplit(out_file, "[.]")[[1]], n = -1), collapse = "."), ".vrt")
    catch <- gdalUtils::gdalbuildvrt(mod_files[f], temp.env, te = c(ext@xmin, ext@ymin, ext@xmax, ext@ymax))
    x <- stack(temp.env)
    if (!is.na(grep(".vrt", tolower(temp.env))[1])) {
      file.rename(temp.env, out_file)
    } else {
      raster::writeRaster(x, ...)
    }
  }
  stopCluster(c1)
}

.mask_aoi <- function(in_dir, aoi, out_dir, cores=NA) {

  # check if output directory exists, otherwise create it
  if (!dir.exists(out_dir)) try(dir.create(out_dir))
  logging::logdebug(paste0("Masked image directory set: ", out_dir))

  # get all files that should be maskeds
  in_files <- list.files(in_dir, pattern = ".*M(O|Y)D.*.tif", full.names = T, no.. = T)

  # setup parallel processing
  if (is.na(cores)) cores <- parallel::detectCores() - 1
  c1 <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(c1)
  logging::logdebug(paste0("Set up parallel processing on ", cores, " cores"))

  # Convert AOI to Spatial
  if (!inherits(aoi, "sf")) stop("AOI must be of type sf.")
  aoi_mask <- try(sf::as_Spatial(aoi))

  # Execute parallel masking
  logging::logdebug(paste0("Masking ", length(in_files), " using specified AOI to ", out_dir))
  foreach::foreach(f = 1:length(in_files), .packages = c("raster", "reproducible")) %dopar% {
    in_rast <- raster::raster(in_files[f])
    out_file <- file.path(out_dir, paste0(strsplit(basename(in_files[f]), ".tif")[[1]][1], ".maskaoi.tif"))
    masked_rast <- reproducible::fastMask(in_rast, aoi_mask)
    raster::writeRaster(masked_rast, out_file, "GTiff")
  }
  stopCluster(c1)
}
