// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_spikeMedian
NumericVector rcpp_spikeMedian(const NumericVector y, const NumericVector w, const int ypts, const int w_min, const int spk);
RcppExport SEXP _phenoRS_rcpp_spikeMedian(SEXP ySEXP, SEXP wSEXP, SEXP yptsSEXP, SEXP w_minSEXP, SEXP spkSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type w(wSEXP);
    Rcpp::traits::input_parameter< const int >::type ypts(yptsSEXP);
    Rcpp::traits::input_parameter< const int >::type w_min(w_minSEXP);
    Rcpp::traits::input_parameter< const int >::type spk(spkSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_spikeMedian(y, w, ypts, w_min, spk));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_phenoRS_rcpp_spikeMedian", (DL_FUNC) &_phenoRS_rcpp_spikeMedian, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_phenoRS(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}