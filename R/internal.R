.getMODIS_date <- function(file, pos1=10, pos2=16, format="%Y%j") {
  return(as.Date(substr(basename(file), pos1, pos2), format = format))
}

.getMODIS_datestr <- function(date, format='%Y%j') {
  return(as.character(strftime(date, format = format)))
}

.getMODIS_tile <- function(file, pos1=18, pos2=23) {
  tile_info <- substr(basename(file), pos1, pos2)
  h <- as.numeric(substr(tile_info, 2, 3))
  v <- as.numeric(substr(tile_info, 5, 6))

  return(c(h, v))
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
