#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;


// predict
arma::mat predict(arma::mat const& X, arma::mat const& W1, arma::mat const& W2, arma::mat const& b1, arma::mat const& b2);
RcppExport SEXP nnetwork_predict(SEXP XSEXP, SEXP W1SEXP, SEXP W2SEXP, SEXP b1SEXP, SEXP b2SEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< arma::mat const& >::type X(XSEXP);
  Rcpp::traits::input_parameter< arma::mat const& >::type W1(W1SEXP);
  Rcpp::traits::input_parameter< arma::mat const& >::type W2(W2SEXP);
  Rcpp::traits::input_parameter< arma::mat const& >::type b1(b1SEXP);
  Rcpp::traits::input_parameter< arma::mat const& >::type b2(b2SEXP);
  rcpp_result_gen = Rcpp::wrap(predict(X, W1, W2, b1, b2));
  return rcpp_result_gen;
  END_RCPP
}