#' @title Modified Hampel Median Filter.
#'
#' @param v numeric vector. Vegetation index values.
#' @param w numeric vector. Initial weights.
#' @param ypts numeric. The number of values per year. Used to determine the
#'             moving window size.
#' @param w_min numeric. Specifies the weight that should be assigned to outliers.
#' @param spk numeric. The Spike parameter. the higher, the more values are classified
#'            outliers.
#'
#' @details #TODO
#'
#' @author Sandro Groth
#'
#' @references Hampel F. R., ”The influence curve and its role in robust estimation,”
#'             Journal of the American Statistical Association, 69, 382–393, 1974
#'
#'             Eklundh, L., and Jönsson, P., 2017, TIMESAT 3.3 with seasonal trend
#'             decomposition and parallel processing - Software Manual.
#'             Lund University, 92 pp.
#'
#' @examples
#' ## Create a random normal distribution with outliers.
#' values <- c(rnorm(19), 50, rnorm(19), -50, rnorm(20))
#'
#' ## set all initial weights to 1
#' init_weights <- rep(1, 60)
#'
#' ## get the spike filtered weights
#' filtered_weights <- spike_median(values, init_weights, 20, 0, 2)
#'
#' @export
#'
spike_median <- function(v, w, ypts, w_min=0, spk=2) {

  n  <- length(v)
  w0 <- w
  s  <- round(ypts / 7)
  k  <- 1.4826

  for (i in seq(s + 1, n - s, 1)) {
    md <- median(v[(i - s):(i + s)], na.rm = TRUE)
    me <- mean(c(v[i - 1], v[i + 1]), na.rm = TRUE)
    mx <- max(c(v[i - 1], v[i + 1]), na.rm = TRUE)
    ctf <- spk * (k * median(abs(v[(i - s):(i + s)] - md), na.rm = TRUE))
    if (abs(v[i] - md) > ctf & (v[i] < (me - ctf) | v[i] > (mx + ctf))) {
        w0[i] <- w_min
      }
  }

  w0
}
