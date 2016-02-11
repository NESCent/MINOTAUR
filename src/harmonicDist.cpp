
#include <Rcpp.h>
#include "harmonicDist.h"

using namespace std;

//------------------------------------------------
// calculates distance between observations as the harmomic mean distance of every point to every other.
// [[Rcpp::export]]
Rcpp::List C_harmonicDist(std::vector< std::vector<double> > data) {

    int dims = int(data.size());
    int n = data[0].size();

    double z, x;
    vector<double> distance(n);

    for (int i=0; i<n; i++) {
        z = 0;
        for (int j=0; j<n; j++) {
            if (i==j)
                continue;
            x = 0;
            for (int k=0; k<dims; k++) {
                x += (data[k][i]-data[k][j])*(data[k][i]-data[k][j]);
            }
            z += 1.0/sqrt(x);
        }
        distance[i] = double(n)/z;
    }

    // return values
    return Rcpp::List::create(Rcpp::Named("distance")=distance);
}
