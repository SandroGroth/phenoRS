#'
#'
#'
#.add_years <- function() {}

#'
#'
#' @export
#'
.cut_incomplete_seasons <- function(y, d, w, mins) {

  N <- length(y)
  yc <- y
  dc <- d
  wc <- w

  # if first local minimum is not in the first place, cut everything before
  cut_begin <- mins[1]
  if (cut_begin > 1) {
    yc <- yc[cut_begin:N]
    dc <- dc[cut_begin:N]
    wc <- wc[cut_begin:N]
    mins <- mins - cut_begin
  }

  # cut everything from the last minimum
  cut_end <- mins[length(mins)]
  yc <- yc[1:cut_end]
  dc <- dc[1:cut_end]
  wc <- wc[1:cut_end]

  return(list(y=yc, d=dc, w=wc, mins=mins))
}

#'
#' TODO: weighted harmonic modeling
#'
#' @param y
#' @param d
#' @param w
#' @param s_per_year
#'
#' @importFrom rHarmonics harmonics_fun
#' @importFrom Rcpp sourceCpp evalCpp
#'
#' @useDynLib phenoRS
#'
#' @export
#'
divide_seasons <- function(y, d, w, s_per_year = 1, amplitude_cutoff = 2000) {

  y0 <- y
  d0 <- d

  # fit the data using harmonic modeling
  yh <- harmonics_fun(y0, d0, s_per_year)

  # find all local peaks and minima
  maxs <- rcpp_findPeaks(yh, m = 3)
  mins <- rcpp_findPeaks(-yh, m = 3)

  # amplitude_cutoff
  maxvals <- sapply(maxs, function(x) {yh[x]})
  minvals <- sapply(mins, function(x) {yh[x]})
  if (mean(maxvals) - mean(minvals) < amplitude_cutoff) {
    warning("Amplitude too low. Skipping...")
    return(NA)
  }

  corrected <- .cut_incomplete_seasons(y0, d0, w, mins)
  corrected$maxs <- maxs

  return (corrected)
}
