#' @title Extract subdatasets from .hdf archive
#'
#' @param hdf Required character. Full path of hdf archive.
#' @param sd Required character. Specifies the subdataset thet should be extracted.
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
