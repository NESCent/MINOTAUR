
#ifndef __RgeoProfile__harmonicDist__
#define __RgeoProfile__harmonicDist__

//------------------------------------------------
// calculates distance between observations as the harmomic mean distance of every point to every other. In C++ terms the input to this function is a vector of vectors, but this translates to a list of vectors in R terms. As such, the pure R function harmonicDist() takes a matrix of input and processes it for use with this function. Similarly, this function deals with the output.
Rcpp::List C_harmonicDist(std::vector< std::vector<double> > data);

#endif