
#include <Rcpp.h>
#include "neighborDist.h"

using namespace std;

//------------------------------------------------
//' @name C_neighborDist
//' @title Nearest neighbor distance
//' @description Calculates nearest neighbor distance between all points and a subset of points.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List C_neighborDist(std::vector< std::vector<double> > data, std::vector<int> subset, std::vector< std::vector<double> > S_inv) {

    int dims = int(data.size());
    int n = int(data[0].size());
    int sub_size = int(subset.size());

    double x, y, z;
    vector<double> distance(n);

    for (int i=0; i<n; i++) {
        z = -1;
        for (int j=0; j<sub_size; j++) {
            if (i==subset[j])
                continue;
            x = 0;
            for (int k1=0; k1<dims; k1++) {
                for (int k2=k1; k2<dims; k2++) {
                    y = (data[k1][i]-data[k1][subset[j]])*S_inv[k1][k2]*(data[k2][i]-data[k2][subset[j]]);
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

//------------------------------------------------
//' @name C_neighborDist_partial
//' @title Nearest neighbor distance using chunk of data to enable the use of a progress bar for large datasets
//' @description Equivalent to C_neighborDist, but only performs calculation on chunk of data, allowing calculation to be broken into sections and thus tracked by a progress bar.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List C_neighborDist_partial(std::vector< std::vector<double> > data, std::vector< std::vector<double> > S_inv, int i_start, int i_end) {

    int dims = int(data.size());
    int n = data[0].size();

    double x, y, z;
    vector<double> distance(i_end-i_start+1);

    for (int i=(i_start-1); i<i_end; i++) {
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
        distance[i-i_start+1] = z;
    }

    // return values
    return Rcpp::List::create(Rcpp::Named("distance")=distance);
}
