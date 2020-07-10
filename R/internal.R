.is_gdal_valid <- function() {
  valid <- !is.null(getOption("gdalUtils_gdalPath"))
  if (isFALSE(valid)) {
    gdal_setInstallation()
    valid <- !is.null(getOption("gdalUtils_gdalPath"))
  }
  return(valid)
}

.getMODIS_date <- function(file, pos1=10, pos2=16, format="%Y%j") {
  return(as.Date(substr(basename(file), pos1, pos2), format = format))
}

.getMODIS_datestr <- function(date, format='%Y%j') {
  return(as.character(strftime(date, format = format)))
}

.getMODIS_composite_str <- function(file, pos1=10, pos2=16) {
  return(substr(basename(file), pos1, pos2))
}

.getMODIS_compositeDOY <- function(file, pos1=14, pos2=16) {
  return(as.integer(substr(basename(file), pos1, pos2)))
}

.getMODIS_compositeYear <- function(file, pos1=10, pos2=13) {
  return(as.integer(substr(basename(file), pos1, pos2)))
}

.getMODIS_tile <- function(file, pos1=18, pos2=23) {
  tile_info <- substr(basename(file), pos1, pos2)
  h <- as.numeric(substr(tile_info, 2, 3))
  v <- as.numeric(substr(tile_info, 5, 6))

  return(c(h, v))
}

.getMODIS_tilestr <- function(file, pos1=18, pos2=23) {
  return(substr(basename(file), pos1, pos2))
}

.get_unique_tilestr <- function(files, pos1=18, pos2=23) {
  return(unique(unlist(lapply(files, .getMODIS_tilestr, pos1 = pos1, pos2 = pos2))))
}

.get_sd_idx <- function(hdf_file, sd_type) {
  if(!file.exists(hdf_file)) stop("File not found.")
  sds_list <- gdalUtils::get_subdatasets(hdf_file)
  switch (sd_type,
    NDVI = {
      idx <- grep('.*NDVI', sds_list, value=FALSE)[1]
      if (!is.na(idx)) return(idx) else stop("No NDVI subdataset found.")
    },
    EVI = {
      idx <- grep('.*EVI', sds_list, value=FALSE)[1]
      if (!is.na(idx)) return(idx) else stop("No EVI subdataset found.")
    },
    QA = {
      idx <- grep('.*pixel reliability', sds_list, value=FALSE)[1]
      if (!is.na(idx)) return(idx) else stop("No QA subdataset found.")
    },
    DOY = {
      idx <- grep('.*day of the year', sds_list, value=FALSE)[1]
      if (!is.na(idx)) return(idx) else stop("No DOY subdataset found.")
    },
    stop("Specified subdataset not supported.")
  )
}

.get_prep_doys <- function(files, pos1=5, pos2=7) {
  return(as.numeric(substr(basename(files), pos1, pos2)))
}

.get_prep_year_str <- function(files, pos1 = 1, pos2 = 4) {
  return(as.numeric(substr(basename(files), pos1, pos2)))
}

.get_prep_comp_str <- function(files, pos1= 1, pos2 = 7) {
  return(substr(basename(files), pos1, pos2))
}

#' Converts R data type to GDAL data type
#'
#' @keywords internal
#'
#' @noRd
#'
.dtype_R_to_GDAL <- function(dtype) {
  switch(dtype,
    'INT2S' = 'Int16',
    'INT2U' = 'UInt16',
    'INT4S' = 'Int32',
    'INT4U' = 'UInt32',
    stop("No valid datatype provided.")
  )
}

#' Converts GDAL data type to R data type
#'
#' @keywords internal
#'
#' @noRd
#'
.dtype_GDAL_to_R <- function(dtype) {
  switch(dtype,
    'Int16'  = 'INT2S',
    'UInt16' = 'INT2U',
    'Int32'  = 'INT4S',
    'UInt32' = 'INT4U',
    stop("No valid datatype provided.")
  )
}

#' Converts ENVI metadata datatype to R data type.
#'
#' @references https://www.harrisgeospatial.com/docs/ENVIHeaderFiles.html
#'
#' @keywords internal
#'
#' @noRd
#'
.dtype_ENVI_to_R <- function(dtype) {
  switch(dtype,
    '2'  = 'INT2S',
    '3'  = 'INT4S',
    '4'  = 'FLT4S',
    '5'  = 'FLT8S',
    '12' = 'INT2U',
    '13' = 'INT4U',
    stop("No valid datatype provided.")
  )
}

#' Setup progress bar
#'
#' @keywords internal
#'
#' @noRd
#'
.setup_pb <- function(min, max, do_par) {

  progress_b <- list()
  progress_b$pb <- txtProgressBar(min = min, max = max, style = 3)
  if (isTRUE(do_par)) {
    progress <- function(n) setTxtProgressBar(pb, n)
    progress_b$opts <- list(progress = progress)
  }

  return(progress_b)
}

#' Start SNOW parallel processing cluster
#'
#' @import parallel
#' @import doSNOW
#'
#' @keywords internal
#'
#' @noRd
#'
.start_SNOW <- function(cores=NA) {

  if (is.na(cores)) cores <- detectCores()
  c1 <- makePSOCKcluster(cores)
  registerDoSNOW(c1)

  return(c1)
}

#' Stop SNOW parallel processing cluster
#'
#' @import parallel
#'
#' @keywords internal
#'
#' @noRd
#'
.stop_SNOW <- function(cluster) {

  stopCluster(cluster)

  return(T)
}

#' On package startup
#'
#' @keywords internal
#'
#' @noRd
#'
.onLoad <- function(libname, pkgname) {

  # internal options
  op <- options()
  op.phRS <- list(
    phRS.gsD = list(
      api = list(
        laads = 'https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/'
        # TODO: Other product sources
      )
    ),
    phRS.supported = list(
      products = list(
        MODIS = c('MOD13Q1_V6'), #TODO_ Add more MODIS products
        Landsat = c(),
        Sentinel = c()
      ),
      VIs = c('NDVI',
              'EVI')
    )
  )

  to_set <- !(names(op.phRS) %in% names(op))
  if (any(to_set)) options(op.phRS[to_set])

  invisible()
}
