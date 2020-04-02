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
