#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
NumericVector calc_pval(NumericVector x, NumericVector y) {
   int size = x.size();
   NumericVector out(size);
   
   for(int i = 0; i < size; i++){
     out(i) = mean(y>x(i));
   }
   return out;
}
