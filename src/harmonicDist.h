
#ifndef __RgeoProfile__harmonicDist__
#define __RgeoProfile__harmonicDist__

//------------------------------------------------
// calculates distance between observations as the harmomic mean distance of every point to every other.
Rcpp::List C_harmonicDist(std::vector< std::vector<double> > data);

#endif
