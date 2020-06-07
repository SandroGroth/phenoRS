#'
#' @param y Numeric vector.
#'
#' Unequally distributed TS:
#' https://dsp.stackexchange.com/questions/1676/savitzky-golay-smoothing-filter-for-not-equally-spaced-data
#'
#' @useDynLib phenoRS
#' @importFrom Rcpp sourceCpp evalCpp
#'
#' @export
#'
smooth_regWSavGol <- function(y, w, ypts, half_win = floor(ypts/7), d = 2, iter = 2) {

  if(all(is.na(x))) return(y)

  y_iter   <- y
  w_iter   <- w
  fit <- NULL

  for (i in 1:iter) {

    # execute fitting
    fit_new <- rcpp_regWSavGol(y_iter, w_iter, half_win, d)

    # update weights
    #w_new <- rcpp_updateWeights(y_iter, fit_new, w_iter, i, ypts)

    # adaption to upper envelope
    I <- which(y_iter < fit_new)
    if (length(I) > 0) y_iter[I] <- fit_new[I]

    fit <- fit_new
    w_iter <- w_new

  }

  list(fits=fit, weights=w_iter)

}

smooth_iregWSavGol <- function(y, x, halfwin, d = 2, iter = 2) {

  if(all(is.na(y))) return(y)
}
