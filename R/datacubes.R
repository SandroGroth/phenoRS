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

.check_bin_metadata <- function(xml_files) {

  # Read xml files
  xmls <- lapply(xml_files, xml2::read_xml)

  # Check if all images have the same extent
  lines <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(x, ".//MDI[@key='lines']"))}))
  samples <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(x, ".//MDI[@key='samples']"))}))
  if (!length(unique(lines)) == 1 | !length(unique(samples)) == 1) {stop("Image sizes are differing.")}

  # Check if the data has datatype 'UInt16'
  dtype <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(x, ".//MDI[@key='data_type']"))}))
  if (!all(dtype == 12)) stop("Not all datatypes are UInt16")

  # Check if 1 bands are available
  bands <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(x, ".//MDI[@key='bands']"))}))
  if (!all(bands == 1)) stop("Some images have more than band.")

  # Check if interleave is BIL
  interleave <- unlist(lapply(xmls, function(x) {xml2::xml_text(xml2::xml_find_all(x, ".//MDI key='interleave'"))}))
  if (!all(interleave == 'bil')) stop("Not all images have interleave 'BIL'.")

  # Check File type
  filetype <- unlist(lapply(xmls, function(x) {xml2::xml_text(xml2::xml_find_all(x, ".//MDI key='file_type'"))}))
  if (!all(filetype == 'ENVI Standard')) stop("Not all images are of file typ 'ENVI Standard'.")

  # Check byte order
  byteorder <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(x, ".//MDI key='byte_order'"))}))
  if (!all(byteorder == 0)) stop("Not all images have byteorder of 0.")

  # Check header offset
  headeroffset <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(x, ".//MDI key='header_offset'"))}))
  if (!all(headeroffset == 0)) stop("Not all images have header offset of 0.")

  # Check the NA value
  na_val  <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(xml2::xml_find_all(x,
                                                                                                      ".//PAMRasterBand[@band='1']"), ".//NoDataValue"))}))
  if (length(unique(na_val)) > 1)  stop("Not all images have the same NA value.")

  return(list(lines        = unique(lines),
              samples      = unique(samples),
              dtype        = unique(dtype),
              interleave   = unique(interleave),
              filetype     = unique(filetype),
              byteorder    = unique(byteorder),
              headeroffset = unique(headeroffset),
              na_val       = unique(na_val)))
}

