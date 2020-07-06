#' @title Valid range adaption
#'
#' @param y
#' @param y_min
#' @param y_max
#'
#' @name .adapt_range
#' @export
#'
.adapt_range <- function(y, y_min, y_max) {
  y0 <- y
  outl_min <- y0 < y_min
  outl_max <- y0 > y_max
  y0[outl_min] <- y_min
  y0[outl_max] <- y_max

  return(y0)
}


#' @title Check input time series.
#'
#' @description Function for checking the validity of an input time series for further processing.
#'
#' @usage
#'
#' @param y
#' @param d
#' @param w
#' @param valid_min
#' @param amplitude_cutoff
#' @param y_min
#' @param y_max
#' @param w_min
#' @param approx_spike
#'
#' @details TODO
#'
#' @return TODO
#'
#' @author Sandro Groth
#'
#' @importFrom zoo na.approx
#'
#' @name check_ts
#' @export
#'
check_ts <- function(y, d, w, comp_d, valid_min, y_min = 200, y_max = 10000, w_min = 0,
                     approx_spike = TRUE) {

  # Input checks
  #TODO

  N  <- length(y)
  y0 <- y
  d0 <- d
  w0 <- w

  # check if TS has different lengths
  if (length(d) != N | length(w) != N | length(comp_d) != N) {
    warning("TS elements have different lengths. Skipping...")
    return(list(y=rep(NA, N), d=rep(NA, N), w=rep(NA, N)))
  }

  # check if TS contains too much missing or bad values
  valids <- y[!is.na(y) & !is.na(d) & !is.na(w) & w != w_min]
  if (length(valids) / N < valid_min) {
    warning("TS contains too many missing values. Skipping...")
    return(list(y=rep(NA, N), d=rep(NA, N), w=rep(NA, N)))
  }

  # NA approximation
  # since these values were not actually observed, the corresponding weight is minimal.
  w0[is.na(y) | is.na(w) | is.na(d)] <- w_min
  y0 <- zoo::na.approx(y, na.rm = FALSE)
  d0 <- round(zoo::na.approx(d, na.rm = FALSE))

  # Range adjustments
  if (isTRUE(approx_spike)) {
    y0[y0 < y_min] <- NA
    w0[y0 < y_min] <- w_min
    y0 <- zoo::na.approx(y0, na.rm = FALSE)

    y0[y0 > y_max] <- NA
    w0[y0 > y_max] <- w_min
    y0 <- zoo::na.approx(y0, na.rm = FALSE)
  }

  # if any NA are left, set it to y_min and weight it minimal
  w0[is.na(y0)] <- w_min
  y0[is.na(y0)] <- y_min

  y0 <- .adapt_range(y0, y_min, y_max)

  # remove duplicates
  # TODO aggregate duplicates by mean value
  duplic <- duplicated(d0)
  y0 <- y0[!duplic]
  d0 <- d0[!duplic]
  w0 <- w0[!duplic]
  comp_d <- comp_d[!duplic]

  # doy to date conversion
  dates <- get_real_dates(d0, comp_d)

  return(list(y=y0, d=dates, w=w0))
}
