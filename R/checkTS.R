check_ts <- function(v, max_data_gap, internal_min) {

  # Interpolate NA value, when the data gap is not greater than specified
  v <- zoo::na.approx(v, maxgap = max_data_gap)

  # Assign internal_min to gaps greater than max_data_gap
  v[is.na(v)] <- internal_min

  v
}
