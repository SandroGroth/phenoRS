#include <Rcpp.h>
using namespace Rcpp;

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

  NumericVector r(1);
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
        if (x(j) > x(j + 1)) {
          isGreatest = false;
        }
      }

      if (isGreatest) {
        if (foundMax > 0) {
          r.insert(0, double(i + 2));
        } else {
          r(0) = double(i + 2);
        }
        foundMax++;
      } else {
        isGreatest = true;
      }
    }
  }

  return r;
}
