
#include <Rcpp.h>
#include "kernelDist.h"

using namespace std;

//------------------------------------------------
// calculates kernel density of all points from all others.
// [[Rcpp::export]]
Rcpp::List C_kernelDist(std::vector< std::vector<double> > data, double sigma2) {

    int dims = int(data.size());
    int n = data[0].size();
    double C1 = 0.5/sigma2;
    double C2 = 2*log(double(n)) + dims*log(6.283185*sigma2);

    double z, x;
    vector<double> distance(n);

    for (int i=0; i<n; i++) {
        z = 0;
        for (int j=0; j<n; j++) {
            x = 0;
            for (int k=0; k<dims; k++) {
                x += (data[k][i]-data[k][j])*(data[k][i]-data[k][j]);
            }
            z += exp(-x*C1);
        }
        distance[i] = C2 - 2*log(z);
    }

    // return values
    return Rcpp::List::create(Rcpp::Named("distance")=distance);
}
