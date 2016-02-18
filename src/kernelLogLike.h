
#ifndef __RgeoProfile__kernelLogLike__
#define __RgeoProfile__kernelLogLike__

//------------------------------------------------
// calculates overall likelihood of data for given kernel bandwidth. Likelihood of point i is equal
// to kernel density from all other points j!=i, and likelihood of overall data is product over i.
Rcpp::List C_kernelLogLike(std::vector< std::vector<double> > data, double sigma2);

#endif
