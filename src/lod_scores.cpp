#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
NumericMatrix lod_scores(NumericMatrix freqARR, NumericMatrix allARR, int num_ind,
double k0, double k1) {
  int nrow = allARR.nrow(), ncol = allARR.ncol();
  double c1, c2, c3, c4;
  NumericMatrix lod(nrow,nrow);
  for(int i=0; i < num_ind;i++){
    for(int j=std::min((i+1),num_ind-1); j < num_ind; j++){
      lod(i,j) = 1.0;
      for(int locus = 0; 2*locus < ncol;locus++){
        if(allARR(i,2*locus+1) == allARR(j,2*locus+1)){
          c1 = double(1);
        } else {
          c1 = double(0);
        }
        if(allARR(i,2*locus+1) == allARR(j,2*locus)){
          c2 = double(1);
        } else {
          c2 = double(0);
        }
        
        if(allARR(i,2*locus) == allARR(j,2*locus+1)){
          c3 = double(1);
        } else {
          c3 = double(0);
        }
        
        if(allARR(i,2*locus) == allARR(j,2*locus)){
          c4 = double(1);
        } else {
          c4 = double(0);
        }
        
        
        lod(i,j) =  
            lod(i,j)*(((c1+c2)/(4.0*freqARR(i,2*locus+1))+
              (c3+c4)/(4.0*freqARR(i,2*locus)))*k1 + k0);
        
      }
      if(lod(i,j)==0){
        lod(i,j) = -999;
      } else {
        lod(i,j) = log10(lod(i,j));
      }
      lod(j,i) = lod(i,j);
    }
  }  
  return lod;
}
