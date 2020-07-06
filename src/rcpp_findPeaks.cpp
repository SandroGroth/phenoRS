#include <Rcpp.h>
using namespace Rcpp;

// Helper function to reverse the order of an vector for correct return to R.
//
// Reference: Eddelbuettel, D. (2012): Reversing a Vector. Rcpp Gallery.
//            https://gallery.rcpp.org/articles/reversing-a-vector/
//
// [[Rcpp::export]]
NumericVector rcpp_vecRev(NumericVector x) {
  NumericVector revX = clone<NumericVector>(x);
  std::reverse(revX.begin(), revX.end());
  ::Rf_copyMostAttrib(x, revX);
  return revX;
}

// Helper function to return the sign of a given real value double.
// Modified from caseyk at StackExcnange:
// https://stats.stackexchange.com/a/230124
//
// [[Rcpp::export]]
double rcpp_signDouble(double x) {
  double r = 0;
  if (x > 0) {r = 1;}
  if (x < 0) {r = -1;}
  return r;
}


// Find all local peaks.
//
// Function to find local peaks in a time series vector.
// Modified from an answer by caseyk on StackExchange:
// https://stats.stackexchange.com/a/230124
//
// [[Rcpp::export]]
NumericVector rcpp_findPeaks(NumericVector x, int m = 3) {

  int size = x.size();

  int lb = 0; // left bound
  int rb = 0; // right bound

  bool isGreatest = true;

  NumericVector ret(1);
  int foundMax = 0;

  for (int i = 0; i < (size - 2); ++i) {
    if (rcpp_signDouble(x(i + 2) - x(i + 1)) - rcpp_signDouble(x(i + 1) - x[i]) < 0) {
      lb = i - m - 1;
      if (lb < 0) {
        lb = 0;
      }
      rb = i + m + 1;
      if (rb >= (size - 2)) {
        rb = (size - 3);
      }
      for (int j = lb; j < rb; ++j) {
        if (x(j) > x(i + 1)) {
          isGreatest = false;
        }
      }

      if (isGreatest) {
        if (foundMax > 0) {
          ret.insert(0, double(i + 2));
        } else {
          ret(0) = double(i + 2);
        }
        foundMax++;
      } else {
        isGreatest = true;
      }
    }
  }

  ret = rcpp_vecRev(ret);

  return ret;
}
