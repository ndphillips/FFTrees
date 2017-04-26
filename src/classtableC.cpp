#include <Rcpp.h>
using namespace Rcpp;

//' classtableC
//'
//' Calculates several classification statistics from binary prediction and criterion (e.g.; truth) vectors
//' @param pred A binary vector of predictions
//' @param crit A binary vector of criterion (true) values
//' @export
// [[Rcpp::export]]
List classtableC(LogicalVector pred,
                 LogicalVector crit) {

  int n = pred.size();
  int correct = 0;
  double hi = 0;
  double fa = 0;
  double cr = 0;
  double mi = 0;

  for (int i = 0; i < n; ++i) {

    if(pred[i] == true & crit[i] == true) {

      hi++;

    }

    if(pred[i] == true & crit[i] == false) {

      fa++;

    }

    if(pred[i] == false & crit[i] == true) {

      mi++;

    }

    if(pred[i] == false & crit[i] == false) {

      cr++;

    }

    if(pred[i] == crit[i]) {correct++;};

  }

  NumericVector sens = NumericVector::create(double(hi) / (double(hi) + double(mi))) ;
  NumericVector spec = NumericVector::create(double(cr) / (double(cr) + double(fa))) ;


  double bacc = (sens[0] + spec[0]) / 2;
  NumericVector dprime = NumericVector::create(qnorm(sens)[0] + qnorm(spec)[0]);

  // if sens is NA, set bacc to spec / 2

  if((sens[0] != sens[0]) && (spec[0] == spec[0])) {

    bacc = (spec[0]) / 2 ;
    dprime = NumericVector::create(NA_INTEGER);

  }


  // if spec is NA

  if((spec[0] != spec[0]) && (sens[0] == sens[0])) {

    bacc = (sens[0]) / 2 ;
    NumericVector dprime = NumericVector::create(NA_INTEGER);

  }

  // if sens and spec are NA

  if((spec[0] != spec[0]) && (sens[0] != sens[0])) {

    bacc = NA_INTEGER;
    NumericVector dprime = NumericVector::create(NA_INTEGER);

  }

  double acc = (hi + cr) / n ;

  NumericVector far = NumericVector::create(1 - spec[0]) ;


  List out = List::create(
    Named("n") = n,
    Named("hi") = hi,
    Named("mi") = mi,
    Named("fa") = fa,
    Named("cr") = cr,
    Named("sens") = sens,
    Named("spec") = spec,
    Named("far") = far,
    Named("acc") = acc,
    Named("bacc") = bacc,
    Named("dprime") = dprime);


  return out;
}
