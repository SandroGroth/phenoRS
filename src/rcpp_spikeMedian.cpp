#include <Rcpp.h>
using namespace Rcpp;

//'
//' @export
// [[Rcpp::export]]
NumericVector rcpp_spikeMedian(const NumericVector y, const NumericVector w,
                               const int ypts, const int w_min = 0, const int spk = 2) {

  // Init
  int n = y.length();
  NumericVector w0 = w;
  int s = floor(ypts / 7);
  const double k = 1.4826;

  for (int i = s + 1; i <= n - s; i++) {
    NumericVector md_tmp(s, NA_REAL);
    for (int j = 0; j < s; j++) {
      md_tmp[j] = y[i - s + j];
    }

    NumericVector me_tmp(3, NA_REAL);
    for (int j = 0; j < 3; j++) {
      me_tmp[j] = y[i - 3 + j];
    }

    double md = median(md_tmp);
    double me = mean(me_tmp);
    double mx = max(me_tmp);

    NumericVector md_abs(s, NA_REAL);
    for (int j = 0; j < s; j++) {
      md_abs[j] = md_tmp[j] - md;
    }

    double ctf = spk * (k * median(md_abs));
    if (abs(y[i] - md) > ctf & (y[i] < (me - ctf) | y[i] > (mx + ctf))) {
      w0[i] = w_min;
    }
  }

  return w0;
}
