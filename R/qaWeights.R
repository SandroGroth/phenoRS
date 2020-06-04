#' Get weights based on MODIS summary QA values
#'
#' @export
#'
MODIS_summary_qa <- function(qa_val, w_min = 0, w_med = 0.5, w_max = 1) {

  w <- rep(0, length(qa_val))

  w[qa_val == 0]              <- w_max  # good
  w[qa_val == 1]              <- w_med  # marginal
  w[qa_val > 1 & qa_val < 4]  <- w_min  # snow/ice, cloudy

  w
}
