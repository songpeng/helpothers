#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
int callFunction(int rownum, Function f) {
  for(int i = 1; i <= rownum; i++){
    f(i);
  }
  return 1;
}
