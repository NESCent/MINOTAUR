
#include <Rcpp.h>
#include "harmonicDist.h"

using namespace std;

//------------------------------------------------
// calculates distance between observations as the harmomic mean distance of every point to every other.
// [[Rcpp::export]]
Rcpp::List C_harmonicDist(std::vector< std::vector<double> > data, std::vector< std::vector<double> > S_inv) {

    int dims = int(data.size());
    int n = data[0].size();
    
    double x, y, z;
    vector<double> distance(n);

    for (int i=0; i<n; i++) {
        z = 0;
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
            z += 1.0/sqrt(x);
        }
        distance[i] = double(n)/z;
    }

    // return values
    return Rcpp::List::create(Rcpp::Named("distance")=distance);
}

//------------------------------------------------
// equivalent to C_harmonicDist, but only performs calculation on chunk of data, allowing calculation to be broken into sections and thus tracked by a progress bar
// [[Rcpp::export]]
Rcpp::List C_harmonicDist_partial(std::vector< std::vector<double> > data, std::vector< std::vector<double> > S_inv, int i_start, int i_end) {
    
    int dims = int(data.size());
    int n = data[0].size();
    
    double x, y, z;
    vector<double> distance(i_end-i_start+1);
    
    for (int i=(i_start-1); i<i_end; i++) {
        z = 0;
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
            z += 1.0/sqrt(x);
        }
        distance[i-i_start+1] = double(n)/z;
    }
    
    // return values
    return Rcpp::List::create(Rcpp::Named("distance")=distance);
}