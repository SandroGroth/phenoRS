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

#' @title Mosaic tiles with same acquisition date.
#'
#' @description TODO
#' @details TODO
#'
#' @param date_str
#' @param search_dir
#' @param out_file
#' @param driver
#' @param dtype
#'
#' @importFrom tools file_ext
#' @importFrom gdalUtils mosaic_rasters
#'
#' @export
#'
mosaic_tiles <- function(date_str, out_file, search_dir, driver = 'GTiff', dtype = 'SInt16') {

  # search for files with similar date string in search_dir
  m_files <- list.files(search_dir, pattern = paste0('.\\A', date_str, "\\."), full.names = T, no.. = T) #TODO: support other formats
  if (length(m_files) == 0) stop("No files for mosaicing found.")
  if (length(m_files) == 1) {
    file.rename(from = m_files[1], to = out_file)
    return(F)
  }

  mosaic_rasters(m_files, out_file, of = driver, ot = dtype)
  return(T)
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
            any(grepl('EPSG:', out_proj)),
            any(grepl('proj=', out_proj)),
            any(grepl('.prf', out_proj))))) {
    stop("Non-valid output projection provided.")
  }

  # execute reprojection
  if (isTRUE(gdal)){
    gdalwarp(in_file, out_file, t_srs = out_proj, of = 'GTiff', ot = dtype)
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
#' @importFrom raster extent stack writeRaster
#'
#' @export
#'


#' @title Raster Masking
#'
#' @description TODO
#' @details TODO
#'
#' @importFrom gdalUtils gdalwarp
#'
#' @export
#'


#' @title Raster to ENVI conversion
#'
#' @description TODO
#' @details TODO
#'
#' @importFrom gdalUtils gdal_translate
#'
#' @export
#'
