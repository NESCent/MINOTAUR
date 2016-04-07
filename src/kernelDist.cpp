
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
    double C2 = 2*log(double(n-1)) + dims*log(6.283185*sigma2);

    double z, x;
    vector<double> distance(n);

    for (int i=0; i<n; i++) {
        z = log(0.0);
        for (int j=0; j<n; j++) {
            if (i==j)
                continue;
            x = 0;
            for (int k=0; k<dims; k++) {
                x += (data[k][i]-data[k][j])*(data[k][i]-data[k][j]);
            }
            // the next if statement does z' = log(exp(z) + exp(-x*C1)) while avoiding underflow/overflow issues.
            if ((-x*C1)>z) {
                z = -x*C1 + log(1 + exp(z + x*C1));
            } else {
                z = z + log(1 + exp(-x*C1 - z));
            }
        }
        distance[i] = C2 - 2*z;
    }

    // return values
    return Rcpp::List::create(Rcpp::Named("distance")=distance);
}
