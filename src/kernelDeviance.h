
#ifndef __RgeoProfile__kernelDeviance__
#define __RgeoProfile__kernelDeviance__

//------------------------------------------------
// calculates overall deviance (-2*log-likelihood) for given kernel bandwidth. Likelihood of point i is equal
// to kernel density from all other points in subset, and likelihood of overall data is product over i.
Rcpp::List C_kernelDeviance(std::vector< std::vector<double> > data, double sigma2, std::vector< std::vector<double> > S_inv);

//------------------------------------------------
// equivalent to C_kernelDeviance, but only performs calculation on chunk of data, allowing calculation to be broken into sections and thus tracked by a progress bar
Rcpp::List C_kernelDeviance(std::vector< std::vector<double> > data, std::vector<int> subset, double sigma2, std::vector< std::vector<double> > S_inv, int i_start, int i_end);

#endif
