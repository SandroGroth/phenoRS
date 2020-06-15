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
  sd_names <- get_subdatasets(hdf)
  if (sd == 'DOY') sd <- 'day of the year'
  if (sd == 'QA') sd <- 'pixel reliability'
  if (length(grep(sd, sd_names, value = T)) < 1) stop("Specified sd not found in .hdf subdatasets.")

  r_sd <- raster(readGDAL(grep(sd, sd_names, value = T), as.is = T, silent = T))

  return(r_sd)
}

#' @title Correct DOY layer of extracted MODIS raster objects.
#'
#' @description TODO
#'
#' @param r_obj Required raster object. Holds MODIS DOY as integers.
#' @param comp_year Required integer. Year of the MODIS composite.
#'
#' @importFrom lubridate leap_year
#'
#' @export
#'
correct_doy <- function(r_obj, comp_year) {

  if (isTRUE(leap_year(comp_year))) {
    r_obj[r_obj < 12] <- r_obj[r_obj < 12] + 366
  } else {
    r_obj[r_obj < 13] <- r_obj[r_obj < 13] + 365
  }

  return(r_obj)

}

#' @title Image Reprojection
#'
#' @description TODO
#' @details TODO
#'
#' @importFrom gdalUtils gdalwarp
#' @importFrom raster raster projectRaster writeRaster
#'
#' @export
#'
reproject <- function(in_file, out_file, out_proj, driver = 'GTiff', dtype = 'SInt16',
                      gdal = TRUE, overwrite = TRUE) {

  # check CRS
  if (!any(c(inherits(out_proj, c("CRS", "sp")),
            any(grepl('EPSG:', out_proj@projargs)),
            any(grepl('proj=', out_proj@projargs)),
            any(grepl('.prf', out_proj@projargs))))) {
    stop("Non-valid output projection provided.")
  }

  # execute reprojection
  if (isTRUE(gdal)){
    gdalwarp(in_file, out_file, t_srs = out_proj@projargs, of = 'GTiff', ot = dtype)
  } else {
    r_in <- raster(in_file)
    r_pj <- projectRaster(r_in, crs = out_proj)
    writeRaster(r_prj, out_file, datatype = dtype, format = driver, overwrite = overwrite)
  }

  return(TRUE)

}

#' @title Fast Raster Cropping
#'
#' @description TODO
#' @details TODO
#'
#' @importFrom gdalUtils gdalbuildvrt
#' @importFrom raster stack writeRaster
#'
#' @references Jakob Schwalb-Willmann and Henrik Fisser (2020). getSpatialData:
#' Get different kinds of freely available spatial datasets. R package version 0.1.0.
#' http://www.github.com/16eagle/getSpatialData/
#'
#' @export
#'
crop_fast <- function(in_file, out_file, aoi_ext, driver = 'GTiff', dtype = 'INT2U') {

  if (!inherits(aoi_ext, c("Extent", "raster"))) stop("AOI extent not of class Extent.")

  tmp_env <- tempfile(fileext = '.vrt')
  catch <- gdalbuildvrt(in_file, tmp_env, te = c(aoi_ext@xmin, aoi_ext@ymin, aoi_ext@xmax, aoi_ext@ymax))
  r <- stack(tmp_env)
  writeRaster(r, out_file, format = driver, datatype = dtype)

  return(TRUE)
}

#' @title Raster Masking
#'
#' @description TODO
#' @details TODO
#'
#' @importFrom gdalUtils gdalwarp
#'
#' @export
#'
mask_fast <- function(in_file, out_file, aoi, driver = 'GTiff', dtype = 'Int16') {

  gdalwarp(in_file, out_file, cutline = aoi, of = driver, ot = dtype)

  return(TRUE)
}

#' @title Raster to ENVI conversion
#'
#' @description TODO
#' @details TODO
#'
#' @importFrom gdalUtils gdal_translate
#' @importFrom tools file_ext
#'
#' @export
#'
to_envi <- function(in_file, out_file, dtype = 'INT2U', co = 'INTERLEAVE=BIL') {

  if (!file_ext(out_file) == 'envi') stop("Output extension must be .envi")

  gdal_translate(in_file, out_file, of = 'ENVI', ot = dtype, co = co)

  return(TRUE)
}
