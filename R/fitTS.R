fitTS <- function(prep_dir, out_dir) {

  # Check if output directory already exists, otherwise create it
  if (!dir.exists(out_dir)) dir.create(out_dir) else stop("Output directory already exists.")

  # check metadata
  xml_files <- list.files(prep_dir, pattern = '.aux.xml', full.names = TRUE, no.. = TRUE)
  xml <- lapply(xml_files, xml2::read_xml)
  meta = .check_bin_metadata(xml)

  # write output metadata
  catch <- unlist(lapply(xml_files, .generate_xml, out_dir = out_dir))
  if (!all(is.null(catch))) stop("Failed to generate all output metadata xml files.")

  hdr_files <- list.files(prep_dir, pattern = '.hdr', full.names =TRUE, no.. = TRUE)
  catch <- unlist(lapply(hdr_files, .generate_hdr, out_dir = out_dir))
  if (!all(is.null(catch))) stop("Failed to generate all output metadata hdr files.")

  # create connections for byte-streaming the image files
  bin_files <- list.files(prep_dir, pattern = '\\.bin$', full.names = TRUE, no.. = TRUE)
  cons <- lapply(bin_files, function(x) {file(x, 'rb')})

  # create connections for output image files
  out_files <- unlist(lapply(bin_files, function(x) {
    out_file <- file.path(out_dir, paste0(strsplit(basename(x), '_')[[1]][1], "_fit.bin"))
    if (!isTRUE(file.create(out_file))) stop(paste0("Failed to create output file ", out_file))
    return(out_file)
  }))
  out_cons <- lapply(out_files, function(x) {file(x, 'wb')})

  # loop trough all rows
  for (i in 1:meta$lines) {

    # Extract all ts values of the current line for each band
    # Each readBin call will stream the next line chunk from the con and return it as a vector
    c_vi  <- lapply(cons, function(x) {readBin(x, "integer", size = 2, signed = FALSE, n = meta$samples)})
    c_doy <- lapply(cons, function(x) {readBin(x, "integer", size = 2, signed = FALSE, n = meta$samples)})
    c_qa  <- lapply(cons, function(x) {readBin(x, "integer", size = 2, signed = FALSE, n = meta$samples)})

    # Loop through all pixels of the extrated row
    for (j in 1:meta$samples) {
      ts_vi  <- stats::ts(unlist(lapply(c_vi, function(x) {x[j]})), start = c(2017, 1), frequency = 46)
      ts_doy <- stats::ts(unlist(lapply(c_doy, function(x) {x[j]})), start = c(2017, 1), frequency = 46)
      ts_qa  <- stats::ts(unlist(lapply(c_qa, function(x) {x[j]})), start = c(2017, 1), frequency = 46)

      # Set binary NA values to R NA value
      ts_vi  <- dplyr::na_if(ts_vi, meta$na_vi)
      ts_doy <- dplyr::na_if(ts_doy, meta$na_doy)
      ts_qa  <- dplyr::na_if(ts_qa, meta$na_qa)

      if (!all(is.na(ts_vi))) {

        # All Processing steps
        bla <- 0
      }
    }
    print(i)
  }
  catch <- lapply(cons, close)
  catch <- lapply(out_cons, close)
}

.check_bin_metadata <- function(xmls) {

  # Check if all images have the same extent
  lines <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(x, ".//MDI[@key='lines']"))}))
  samples <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(x, ".//MDI[@key='samples']"))}))
  if (!length(unique(lines)) == 1 | !length(unique(samples)) == 1) {stop("Image sizes are differing.")}

  # Check if the data has datatype 'UInt16'
  dtype <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(x, ".//MDI[@key='data_type']"))}))
  if (!all(dtype == 12)) stop("Not all datatypes are UInt16")

  # Check if 3 bands are available
  bands <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(x, ".//MDI[@key='bands']"))}))
  if (!all(bands == 3)) stop("Not all images have 3 bands.")

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
  na_vi  <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(xml2::xml_find_all(x,
                                            ".//PAMRasterBand[@band='1']"), ".//NoDataValue"))}))
  na_doy <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(xml2::xml_find_all(x,
                                            ".//PAMRasterBand[@band='2']"), ".//NoDataValue"))}))
  na_qa  <- unlist(lapply(xmls, function(x) {xml2::xml_integer(xml2::xml_find_all(xml2::xml_find_all(x,
                                            ".//PAMRasterBand[@band='3']"), ".//NoDataValue"))}))
  if (length(unique(na_vi)) > 1)  stop("Not all VI bands have the same NA value.")
  if (length(unique(na_doy)) > 1) stop("Not all DOY bands have the same NA value.")
  if (length(unique(na_qa)) > 1)  stop("Not all QA bands have the same NA value.")


  return(list(lines        = unique(lines),
              samples      = unique(samples),
              dtype        = unique(dtype),
              interleave   = unique(interleave),
              filetype     = unique(filetype),
              byteorder    = unique(byteorder),
              headeroffset = unique(headeroffset),
              na_vi        = unique(na_vi),
              na_doy       = unique(na_doy),
              na_qa        = unique(na_qa)))
}

.generate_xml <- function(in_xml_file, out_dir) {

  # Check if output xml already extists
  out_file <- file.path(out_dir, paste0(strsplit(basename(in_xml_file), '_')[[1]][1], "_fit.bin.aux.xml"))
  if(file.exists(out_file)) stop(paste0(basename(in_xml_file)), " already exists.")

  # Read xml
  out_xml <- xml2::read_xml(in_xml_file)

  # set bands to 2, since output bands will be the fitted vi and doy. qa will be dropped after fitting
  out_xml %>%
    xml2::xml_find_all(".//MDI[@key='bands']") %>%
    xml2::xml_set_text("2")

  # remove also the node for band 3
  out_xml %>%
    xml2::xml_find_all(".//PAMRasterBand[@band='3']") %>%
    xml2::xml_remove()

  return(xml2::write_xml(out_xml, out_file))
}

.generate_hdr <- function(in_hdr_file, out_dir) {

  # Check if output hdr already exists
  out_file <- file.path(out_dir, paste0(strsplit(basename(in_hdr_file), '_')[[1]][1], "_fit.hdr"))
  if(file.exists(out_file)) stop(paste0(basename(in_hdr_file)), " already exists.")

  # read hdr file as dataframe
  out_hdr <- read.table(in_hdr_file, sep = "\t", stringsAsFactors = FALSE)

  # change description to output path
  out_hdr[which(grepl('description', out_hdr$V1))[1] + 1, 1] <- paste0(out_file, '}')
  # change number of bands to 2
  out_hdr[which(grepl('bands', out_hdr$V1))[1], 1] <- 'bands = 3'
  # remove band 3 from band names
  out_hdr[which(grepl('Band 2', out_hdr$V1))[1], 1] <- 'Band 2}'
  out_hdr <- out_hdr[-c(which(grepl('Band 3', out_hdr$V1))[1]), ]

  return(write.table(out_hdr, out_file, append = FALSE, quote = FALSE, row.names = FALSE, col.names = FALSE))
}
