#' @import doParallel
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach
#' @importFrom gdalUtils gdal_translate
#' @importFrom logging loginfo logdebug
#'
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

#' @import doParallel
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach
#' @importFrom gdalUtils mosaic_rasters
#' @importFrom logging loginfo logdebug
#'
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

#' @importFrom logging loginfo logdebug
#'
prepMODIS <- function(in_dir, out_dir, extract_sds=c("ndvi", "qa", "doy"), cores=NA, check_existing=T) {

  logging::loginfo("-----------------------------------------------------------------------------------")
  logging::loginfo("Preprocessing of downloaded MODIS tiles started.")

  # Check if output directory exists, otherwise create it
  if (!dir.exists(out_dir)) try(dir.create(out_dir))
  logging::loginfo((paste0("Output directory set: ", out_dir)))

  # check if all specified sds can be extracted
  supported_sds <- c("ndvi", "evi", "doy", "qa")
  if (!all(extract_sds %in% supported_sds)) stop(paste0("Only following sds supported: ", supported_sds))

  logging::loginfo("-----------------------------------------------------------------------------------")
  logging::loginfo("Step 1: Extracting data")

  # Check if sub directories exists, otherwise create it
  raw_dir <- file.path(out_dir, "RAW")
  if (!dir.exists(raw_dir)) try(dir.create(raw_dir))
  logging::loginfo(paste0("Raw data directory set: ", raw_dir))
  mosaic_dir <- file.path(out_dir, "MOSAIC")
  if (!dir.exists(mosaic_dir)) try(dir.create(mosaic_dir))
  logging::loginfo(paste0("Mosaic data directory set: ", mosaic_dir))

  # loop trough sds and extract them to subdirectory
  for (i in 1:length(extract_sds)) {
    raw_sub_dir <- file.path(raw_dir, toupper(extract_sds[i]))
    if (!dir.exists(raw_sub_dir)) try(dir.create(raw_sub_dir))
    .extract_sd(in_dir, raw_sub_dir, extract_sds[i], cores, check_existing)

    mosaic_sub_dir <- file.path(mosaic_dir, toupper(extract_sds[i]))
    if (!dir.exists(mosaic_sub_dir)) try(dir.create(mosaic_sub_dir))
    .mosaic_tiles(raw_sub_dir, mosaic_sub_dir, extract_sds[i], cores, check_existing)
  }

  logging::loginfo("All subdatasets successfully extracted.")
}

#prepMODIS("C:\\Projects\\R\\Data\\hdf", "C:\\Projects\\R\\Data\\SDS_Test")
