#ifndef __UTILITIES__
#define __UTILITIES__

#include <Rcpp.h>

Rcpp::List classtableC(Rcpp::LogicalVector pred,
                       Rcpp::LogicalVector crit);


// cpp version of %in%
inline Rcpp::LogicalVector inset(std::vector<std::string> x,
                                 std::vector<std::string> target) {

  int n = x.size();
  Rcpp::LogicalVector out(n);
  for (int i = 0; i < n; ++i) {
    out[i] = std::find(target.begin(), target.end(), x[i]) != target.end();
  }



  return out;
}

#endif // __UTILITIES__
