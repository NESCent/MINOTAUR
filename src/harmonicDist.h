
#ifndef __RgeoProfile__harmonicDist__
#define __RgeoProfile__harmonicDist__

//------------------------------------------------
// calculates distance between observations as the harmomic mean distance of every point from a selected subset of points (subset defaults to all points).
Rcpp::List C_harmonicDist(std::vector< std::vector<double> > data, std::vector<int> subset, std::vector< std::vector<double> > S_inv);

//------------------------------------------------
// equivalent to C_harmonicDist, but only performs calculation on chunk of data, allowing calculation to be broken into sections and thus tracked by a progress bar
Rcpp::List C_harmonicDist_partial(std::vector< std::vector<double> > data, std::vector< std::vector<double> > S_inv, int i_start, int i_end);

#endif
