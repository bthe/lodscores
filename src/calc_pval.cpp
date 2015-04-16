#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
NumericVector calc_pval(NumericVector x, NumericVector y) {
   int xn = x.size(), yn = y.size();
   NumericVector out(xn);
   // iteration variables
   int i, j = 0; //xn - 1;
   for(i = 0; i < xn; i++){
     while(y(j)<x(i)){
       j++;
       if(j>=yn){
         break;
       }
     }
     out(i) = double(yn-j)/double(yn);
     if(j>=yn){
       break;
     }
   }
   return out;
}
