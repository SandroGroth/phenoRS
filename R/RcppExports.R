# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#'
#' @export
rcpp_spikeMedian <- function(y, w, ypts, w_min = 0L, spk = 2L) {
    .Call('_phenoRS_rcpp_spikeMedian', PACKAGE = 'phenoRS', y, w, ypts, w_min, spk)
}

rcpp_wTSM <- function(y, yfit, w, iter, nptperyear, wfact) {
    .Call('_phenoRS_rcpp_wTSM', PACKAGE = 'phenoRS', y, yfit, w, iter, nptperyear, wfact)
}

