// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_vecRev
NumericVector rcpp_vecRev(NumericVector x);
RcppExport SEXP _phenoRS_rcpp_vecRev(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_vecRev(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_signDouble
double rcpp_signDouble(double x);
RcppExport SEXP _phenoRS_rcpp_signDouble(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_signDouble(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_findPeaks
NumericVector rcpp_findPeaks(NumericVector x, int m);
RcppExport SEXP _phenoRS_rcpp_findPeaks(SEXP xSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_findPeaks(x, m));
    return rcpp_result_gen;
END_RCPP
}
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
// rcpp_wTSM
NumericVector rcpp_wTSM(NumericVector y, NumericVector yfit, NumericVector w, int iter, int nptperyear, double wfact);
RcppExport SEXP _phenoRS_rcpp_wTSM(SEXP ySEXP, SEXP yfitSEXP, SEXP wSEXP, SEXP iterSEXP, SEXP nptperyearSEXP, SEXP wfactSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type yfit(yfitSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    Rcpp::traits::input_parameter< int >::type iter(iterSEXP);
    Rcpp::traits::input_parameter< int >::type nptperyear(nptperyearSEXP);
    Rcpp::traits::input_parameter< double >::type wfact(wfactSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_wTSM(y, yfit, w, iter, nptperyear, wfact));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_phenoRS_rcpp_vecRev", (DL_FUNC) &_phenoRS_rcpp_vecRev, 1},
    {"_phenoRS_rcpp_signDouble", (DL_FUNC) &_phenoRS_rcpp_signDouble, 1},
    {"_phenoRS_rcpp_findPeaks", (DL_FUNC) &_phenoRS_rcpp_findPeaks, 2},
    {"_phenoRS_rcpp_spikeMedian", (DL_FUNC) &_phenoRS_rcpp_spikeMedian, 5},
    {"_phenoRS_rcpp_wTSM", (DL_FUNC) &_phenoRS_rcpp_wTSM, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_phenoRS(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
