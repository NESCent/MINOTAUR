
#ifndef __RgeoProfile__neighborDist__
#define __RgeoProfile__neighborDist__

//------------------------------------------------
// calculates distance to nearest neighbor for all points
Rcpp::List C_neighborDist(std::vector< std::vector<double> > data, std::vector< std::vector<double> > S_inv);

//------------------------------------------------
// calculates distance to nearest neighbor for all points
Rcpp::List C_neighborDist_partial(std::vector< std::vector<double> > data, std::vector< std::vector<double> > S_inv, int i_start, int i_end);

#endif
