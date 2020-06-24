#' @title Check VI timeseries
#'
#' @export
#'
check_vi_ts <- function(v, max_data_gap, internal_min) {

  # Interpolate NA value, when the data gap is not greater than specified
  v <- zoo::na.approx(v, maxgap = max_data_gap) # VI

  # Assign internal_min to gaps greater than max_data_gap
  v[is.na(v)] <- internal_min

  v
}

#' @title Check DOY timeseries
#'
#' @export
#'
check_doy_ts <- function(d) {

  # Interpolate NA value
  d <- zoo::na.approx(d)

  d
}
