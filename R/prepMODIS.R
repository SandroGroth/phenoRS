#' @importFrom logging loginfo logdebug
#'
#' TODO: Make cropping optional
#' TODO: new param keep_raw to optionally delete all processing steps
#'
prepMODIS <- function(in_dir, out_dir, extract_sds=c("ndvi", "qa", "doy"), aoi, cores=NA, check_existing=T) {

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
  logging::logdbeug(paste0("Raw data directory set: ", raw_dir))
  mosaic_dir <- file.path(out_dir, "MOSAIC")
  if (!dir.exists(mosaic_dir)) try(dir.create(mosaic_dir))
  logging::logdebug(paste0("Mosaic data directory set: ", mosaic_dir))
  crop_dir <- file.path(out_dir, "CROP")
  if (!dir.exists(crop_dir)) try(dir.create(crop_dir))
  logging::logdebug(paste0("Cropped data directory set: ", crop_dir))
  maskaoi_dir <- file.path(out_dir, "MASK_AOI")
  if (!dir.exists(maskaoi_dir)) try(dir.create(maskaoi_dir))
  logging::logdebug(paste0("AOI masked data directory set: ", maskaoi_dir))

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
  logging::loginfo("-----------------------------------------------------------------------------------")
  logging::loginfo("Step 2: Cropping and masking data to AOI")

  # loop trough sds in mosaic directory
  for (i in 1:length(extract_sds)) {
    mosaic_sub_dir <- file.path(mosaic_dir, toupper(extract_sds[i]))

    # Execute cropping to AOI
    crop_sub_dir <- file.path(crop_dir, toupper(extract_sds[i]))
    if (!dir.exists(crop_sub_dir)) try(dir.create(crop_sub_dir))
    .crop(mosaic_sub_dir, crop_sub_dir, aoi, cores)

    # Execute masking to AOI
    maskaoi_sub_dir <- file.path(maskaoi_dir, toupper(extract_sds[i]))
    if (!dir.exists(maskaoi_sub_dir)) try(dir.create(maskaoi_sub_dir))
    .mask_aoi(crop_sub_dir, aoi, maskaoi_sub_dir, cores)
  }

  logging::loginfo("All subdatasets sucessfully cropped and masked to AOI.")
}

#prepMODIS("C:\\Projects\\R\\Data\\hdf", "C:\\Projects\\R\\Data\\SDS_Test")
