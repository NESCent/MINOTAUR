
#include <Rcpp.h>
#include "neighborDist.h"

using namespace std;

//------------------------------------------------
// calculates distance to nearest neighbor for all points
// [[Rcpp::export]]
Rcpp::List C_neighborDist(std::vector< std::vector<double> > data) {

    int dims = int(data.size());
    int n = data[0].size();

    double z, x;
    vector<double> distance(n);

    for (int i=0; i<n; i++) {
        z = -1;
        for (int j=0; j<n; j++) {
            if (i==j)
                continue;
            x = 0;
            for (int k=0; k<dims; k++) {
                x += (data[k][i]-data[k][j])*(data[k][i]-data[k][j]);
            }
            x = sqrt(x);
            if (z<0 || x<z)
                z = x;
        }
        distance[i] = z;
    }

    // return values
    return Rcpp::List::create(Rcpp::Named("distance")=distance);
}
