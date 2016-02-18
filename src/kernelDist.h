
#ifndef __RgeoProfile__kernelDist__
#define __RgeoProfile__kernelDist__

//------------------------------------------------
// calculates kernel density of all points from all others.
Rcpp::List C_kernelDist(std::vector< std::vector<double> > data, double sigma2);

#endif
