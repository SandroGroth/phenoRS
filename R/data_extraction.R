#' @title Extract subdatasets from .hdf archive
#'
#' @param hdf Required character. Full path of hdf archive.
#' @param sd Required character. Specifies the subdataset that should be extracted.
#'
#' @importFrom gdalUtils get_subdatasets
#' @importFrom raster raster
#' @importFrom rgdal readGDAL
#'
#' @export
#'
extract_hdf <- function(hdf, sd, d_type='INT2S') {

  # input checks
  stopifnot("Input file is not of type .hdf" = endsWith(hdf, ".hdf"))

  # get available subdatasets from archive
  sd_names <- get_subdatasets(x)
  if (length(grep(sd, sd_names, value = T)) == 0) stop("Specified sd not found in .hdf subdatasets.")

  r_sd <- raster(readGDAL(grep(sd, sd_names, value = T), as.is = T))

  return(r_sd)
}

#' @title Correct DOY layer of extracted MODIS raster objects.
#'
#' @description TODO
#'
#' @param r_obj Required raster object. Holds MODIS DOY as integers.
#' @param comp_year Required integer. Year of the MODIS composite.
#' @param comp_doy Required integer. DOY of the MODIS composite.
#'
#' @importFrom lubridate leap_year
#'
#' @export
#'
correct_doy <- function(r_obj, comp_year, comp_doy) {

  if (isTRUE(leap_year(comp_year))) {
    r_obj[r_obj < 12] <- r_obj[r_obj < 12] + 366
  } else {
    r_obj[r_obj < 13] <- r_obj[r_obj < 13] + 365
  }

  return(r_obj)

}
