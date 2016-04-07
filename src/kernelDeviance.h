
#ifndef __RgeoProfile__kernelDeviance__
#define __RgeoProfile__kernelDeviance__

//------------------------------------------------
// calculates overall deviance (-2*log-likelihood) for given kernel bandwidth. Likelihood of point i is equal
// to kernel density from all other points j!=i, and likelihood of overall data is product over i.
Rcpp::List C_kernelDeviance(std::vector< std::vector<double> > data, double sigma2);

#endif
