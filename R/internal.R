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

.get_prep_year_str <- function(files, pos1 = 1, pos2 = 4) {
  return(as.numeric(substr(basename(files), pos1, pos2)))
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
