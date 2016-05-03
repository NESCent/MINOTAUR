
#include <Rcpp.h>
#include "neighborDist.h"

using namespace std;

//------------------------------------------------
// calculates distance to nearest neighbor for all points
// [[Rcpp::export]]
Rcpp::List C_neighborDist(std::vector< std::vector<double> > data, std::vector< std::vector<double> > S_inv) {
    
    int dims = int(data.size());
    int n = data[0].size();
    
    double x, y, z;
    vector<double> distance(n);
    
    for (int i=0; i<n; i++) {
        z = -1;
        for (int j=0; j<n; j++) {
            if (i==j)
                continue;
            x = 0;
            for (int k1=0; k1<dims; k1++) {
                for (int k2=k1; k2<dims; k2++) {
                    y = (data[k1][i]-data[k1][j])*S_inv[k1][k2]*(data[k2][i]-data[k2][j]);
                    if (k1==k2) {
                        x += y;
                    } else {
                        x += 2*y;
                    }
                }
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