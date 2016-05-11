
#ifndef __RgeoProfile__kernelDist__
#define __RgeoProfile__kernelDist__

//------------------------------------------------
// calculates kernel density of all points from all others.
Rcpp::List C_kernelDist(std::vector< std::vector<double> > data, double sigma2, std::vector< std::vector<double> > S_inv);

// calculates kernel density of all points from all others.
Rcpp::List C_kernelDist_partial(std::vector< std::vector<double> > data, double sigma2, std::vector< std::vector<double> > S_inv, int i_start, int i_end);

#endif
