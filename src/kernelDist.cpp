
#include <Rcpp.h>
#include "kernelDist.h"

using namespace std;

//------------------------------------------------
// calculates kernel density of all points from all others.
// [[Rcpp::export]]
Rcpp::List C_kernelDist(std::vector< std::vector<double> > data, double sigma2, std::vector< std::vector<double> > S_inv) {
    
    int dims = int(data.size());
    int n = data[0].size();
    double C1 = 0.5/sigma2;
    double C2 = 2*log(double(n-1)) + dims*log(6.283185*sigma2);
    
    double x, y, z;
    vector<double> distance(n);
    
    for (int i=0; i<n; i++) {
        z = log(0.0);
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

//------------------------------------------------
// equivalent to C_kernelDist, but only performs calculation on chunk of data, allowing calculation to be broken into sections and thus tracked by a progress bar
// [[Rcpp::export]]
Rcpp::List C_kernelDist_partial(std::vector< std::vector<double> > data, double sigma2, std::vector< std::vector<double> > S_inv, int i_start, int i_end) {
    
    int dims = int(data.size());
    int n = data[0].size();
    double C1 = 0.5/sigma2;
    double C2 = 2*log(double(n-1)) + dims*log(6.283185*sigma2);
    
    double x, y, z;
    vector<double> distance(i_end-i_start+1);
    
    for (int i=(i_start-1); i<i_end; i++) {
        z = log(0.0);
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
            // the next if statement does z' = log(exp(z) + exp(-x*C1)) while avoiding underflow/overflow issues.
            if ((-x*C1)>z) {
                z = -x*C1 + log(1 + exp(z + x*C1));
            } else {
                z = z + log(1 + exp(-x*C1 - z));
            }
        }
        distance[i-i_start+1] = C2 - 2*z;
    }
    
    // return values
    return Rcpp::List::create(Rcpp::Named("distance")=distance);
}