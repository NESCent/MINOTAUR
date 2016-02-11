
#include <Rcpp.h>
#include "harmonicDist.h"

using namespace std;

//------------------------------------------------
// calculates distance between observations as the harmomic mean distance of every point to every other. In C++ terms the input to this function is a vector of vectors, but this translates to a list of vectors in R terms. As such, the pure R function harmonicDist() takes a matrix of input and processes it for use with this function. Similarly, this function deals with the output.
// [[Rcpp::export]]
Rcpp::List C_harmonicDist(std::vector< std::vector<double> > data) {
    
    int dims = int(data.size());
    int n = data[0].size();
    
    double h, x;
    vector<double> harmonic(n);
    
    for (int i=0; i<n; i++) {
        h = 0;
        for (int j=0; j<n; j++) {
            if (i==j)
                continue;
            x = 0;
            for (int k=0; k<dims; k++) {
                x += (data[k][i]-data[k][j])*(data[k][i]-data[k][j]);
            }
            h += 1.0/sqrt(x);
        }
        harmonic[i] = double(n)/h;
    }
    
    // return values
    return Rcpp::List::create(Rcpp::Named("harmonic")=harmonic);
}
