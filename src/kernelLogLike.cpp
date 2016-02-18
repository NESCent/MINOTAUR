
#include <Rcpp.h>
#include "kernelLogLike.h"

using namespace std;

//------------------------------------------------
// calculates overall likelihood of data for given kernel bandwidth. Likelihood of point i is equal
// to kernel density from all other points j!=i, and likelihood of overall data is product over i.
// [[Rcpp::export]]
Rcpp::List C_kernelLogLike(std::vector< std::vector<double> > data, double sigma2) {

  int dims = int(data.size());
  int n = data[0].size();
  double C1 = 0.5/sigma2;
  double C2 = -log(double(n)) - 0.5*dims*log(6.283185*sigma2);

  double z, x;
  double logLike = 0;

  for (int i=0; i<n; i++) {
    z = 0;
    for (int j=0; j<n; j++) {
      if (i==j)
        continue;
      x = 0;
      for (int k=0; k<dims; k++) {
        x += (data[k][i]-data[k][j])*(data[k][i]-data[k][j]);
      }
      z += exp(-x*C1);
    }
    logLike += C2 + log(z);
  }

  // return logLike
  return Rcpp::List::create(Rcpp::Named("logLike")=logLike);
}
