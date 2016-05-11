// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// C_harmonicDist
Rcpp::List C_harmonicDist(std::vector< std::vector<double> > data, std::vector< std::vector<double> > S_inv);
RcppExport SEXP MINOTAUR_C_harmonicDist(SEXP dataSEXP, SEXP S_invSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type S_inv(S_invSEXP);
    __result = Rcpp::wrap(C_harmonicDist(data, S_inv));
    return __result;
END_RCPP
}
// C_harmonicDist_partial
Rcpp::List C_harmonicDist_partial(std::vector< std::vector<double> > data, std::vector< std::vector<double> > S_inv, int i_start, int i_end);
RcppExport SEXP MINOTAUR_C_harmonicDist_partial(SEXP dataSEXP, SEXP S_invSEXP, SEXP i_startSEXP, SEXP i_endSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type S_inv(S_invSEXP);
    Rcpp::traits::input_parameter< int >::type i_start(i_startSEXP);
    Rcpp::traits::input_parameter< int >::type i_end(i_endSEXP);
    __result = Rcpp::wrap(C_harmonicDist_partial(data, S_inv, i_start, i_end));
    return __result;
END_RCPP
}
// C_kernelDeviance
Rcpp::List C_kernelDeviance(std::vector< std::vector<double> > data, double sigma2, std::vector< std::vector<double> > S_inv);
RcppExport SEXP MINOTAUR_C_kernelDeviance(SEXP dataSEXP, SEXP sigma2SEXP, SEXP S_invSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type sigma2(sigma2SEXP);
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type S_inv(S_invSEXP);
    __result = Rcpp::wrap(C_kernelDeviance(data, sigma2, S_inv));
    return __result;
END_RCPP
}
// C_kernelDeviance_partial
Rcpp::List C_kernelDeviance_partial(std::vector< std::vector<double> > data, double sigma2, std::vector< std::vector<double> > S_inv, int i_start, int i_end);
RcppExport SEXP MINOTAUR_C_kernelDeviance_partial(SEXP dataSEXP, SEXP sigma2SEXP, SEXP S_invSEXP, SEXP i_startSEXP, SEXP i_endSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type sigma2(sigma2SEXP);
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type S_inv(S_invSEXP);
    Rcpp::traits::input_parameter< int >::type i_start(i_startSEXP);
    Rcpp::traits::input_parameter< int >::type i_end(i_endSEXP);
    __result = Rcpp::wrap(C_kernelDeviance_partial(data, sigma2, S_inv, i_start, i_end));
    return __result;
END_RCPP
}
// C_kernelDist
Rcpp::List C_kernelDist(std::vector< std::vector<double> > data, double sigma2, std::vector< std::vector<double> > S_inv);
RcppExport SEXP MINOTAUR_C_kernelDist(SEXP dataSEXP, SEXP sigma2SEXP, SEXP S_invSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type sigma2(sigma2SEXP);
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type S_inv(S_invSEXP);
    __result = Rcpp::wrap(C_kernelDist(data, sigma2, S_inv));
    return __result;
END_RCPP
}
// C_kernelDist_partial
Rcpp::List C_kernelDist_partial(std::vector< std::vector<double> > data, double sigma2, std::vector< std::vector<double> > S_inv, int i_start, int i_end);
RcppExport SEXP MINOTAUR_C_kernelDist_partial(SEXP dataSEXP, SEXP sigma2SEXP, SEXP S_invSEXP, SEXP i_startSEXP, SEXP i_endSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type sigma2(sigma2SEXP);
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type S_inv(S_invSEXP);
    Rcpp::traits::input_parameter< int >::type i_start(i_startSEXP);
    Rcpp::traits::input_parameter< int >::type i_end(i_endSEXP);
    __result = Rcpp::wrap(C_kernelDist_partial(data, sigma2, S_inv, i_start, i_end));
    return __result;
END_RCPP
}
// C_neighborDist
Rcpp::List C_neighborDist(std::vector< std::vector<double> > data, std::vector< std::vector<double> > S_inv);
RcppExport SEXP MINOTAUR_C_neighborDist(SEXP dataSEXP, SEXP S_invSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type S_inv(S_invSEXP);
    __result = Rcpp::wrap(C_neighborDist(data, S_inv));
    return __result;
END_RCPP
}
// C_neighborDist_partial
Rcpp::List C_neighborDist_partial(std::vector< std::vector<double> > data, std::vector< std::vector<double> > S_inv, int i_start, int i_end);
RcppExport SEXP MINOTAUR_C_neighborDist_partial(SEXP dataSEXP, SEXP S_invSEXP, SEXP i_startSEXP, SEXP i_endSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::vector< std::vector<double> > >::type S_inv(S_invSEXP);
    Rcpp::traits::input_parameter< int >::type i_start(i_startSEXP);
    Rcpp::traits::input_parameter< int >::type i_end(i_endSEXP);
    __result = Rcpp::wrap(C_neighborDist_partial(data, S_inv, i_start, i_end));
    return __result;
END_RCPP
}
