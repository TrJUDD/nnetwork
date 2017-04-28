// Replace the automatically generated RcppExport file, to avoid creation of unwanted function in R

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

// random_sub_mat
arma::mat random_sub_mat(arma::mat const& A);
RcppExport SEXP nnetwork_random_sub_mat(SEXP ASEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< arma::mat const& >::type A(ASEXP);
  rcpp_result_gen = Rcpp::wrap(random_sub_mat(A));
  return rcpp_result_gen;
  END_RCPP
}

// nnetwork
Rcpp::List nnetwork(arma::mat const& X, arma::mat const& Y, arma::mat& W1, arma::mat& W2, arma::mat& b1, arma::mat& b2, int n_iter, double learningrate, int batch_size);
RcppExport SEXP nnetwork_nnetwork(SEXP XSEXP, SEXP YSEXP, SEXP W1SEXP, SEXP W2SEXP, SEXP b1SEXP, SEXP b2SEXP, SEXP n_iterSEXP, SEXP learningrateSEXP, SEXP batch_sizeSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< arma::mat const& >::type X(XSEXP);
  Rcpp::traits::input_parameter< arma::mat const& >::type Y(YSEXP);
  Rcpp::traits::input_parameter< arma::mat& >::type W1(W1SEXP);
  Rcpp::traits::input_parameter< arma::mat& >::type W2(W2SEXP);
  Rcpp::traits::input_parameter< arma::mat& >::type b1(b1SEXP);
  Rcpp::traits::input_parameter< arma::mat& >::type b2(b2SEXP);
  Rcpp::traits::input_parameter< int >::type n_iter(n_iterSEXP);
  Rcpp::traits::input_parameter< double >::type learningrate(learningrateSEXP);
  Rcpp::traits::input_parameter< int >::type batch_size(batch_sizeSEXP);
  rcpp_result_gen = Rcpp::wrap(nnetwork(X, Y, W1, W2, b1, b2, n_iter, learningrate, batch_size));
  return rcpp_result_gen;
  END_RCPP
}
