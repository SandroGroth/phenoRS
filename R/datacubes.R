#' @title Build raster cube.
#'
#' @param files
#' @param xml_files
#'
#' @importFrom raster brick raster
#'
#' @export
#'
build_cube <- function(files, xml_files) {

  # Input checks
  if (!identical(length(files), length(xml_files))) stop("Number of raster files and metadata files not equal.")

  # check metadata
  meta <- .check_bin_metadata(xml_files)

  dc <- brick(sapply(files, function(x) {
    r <- raster(x)
    names(r) <- basename(x)
    return(r)
  }))
  names(dc) <- unlist(lapply(files, function(x) {
    strsplit(basename(x), "_")[[1]][1]
  }))

  return(dc)
}

