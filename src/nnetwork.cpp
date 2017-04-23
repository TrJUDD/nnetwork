
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]



// Used functions, matrix form
arma::mat sigmoid(arma::mat const &A) {
  arma::mat one = ones(size(A)) ;
  return one / (one + exp(-A)) ;
}

arma::mat sigmoid_prime(arma::mat const &A) {
  arma::mat one = ones(size(A)) ;
  return A % (one - A) ;
}

double l2norm(arma::mat const &y1, arma::mat const &y2) {
  return sqrt(accu(y1 - y2)) ;
}


arma::mat l2norm_prime(arma::mat const &y1, arma::mat const &y2) {
  return 2 * (y1 - y2) ;
}


// Forward propagation
void fprop(arma::mat const &X,arma::mat const &W1,
                 arma::mat const &W2, arma::mat const &b1,
                 arma::mat const &b2, arma::mat &U,
                 arma::mat &pred) {
  arma::mat one(X.n_rows , 1, arma::fill::ones) ;
  U = sigmoid(one * b1.t() + X * W1 ) ;
  pred = one * b2.t() + U * W2 ;
}


// Backward propagation
void bprop(arma::mat const &X, arma::mat const &Y,
                 arma::mat const &U, arma::mat const &pred,
                 arma::mat const &W1, arma::mat const &W2,
                 arma::mat const &b1, arma::mat const &b2,
                 arma::mat &dW1, arma::mat &dW2,
                 arma::mat &db1, arma::mat &db2) {
  arma::mat G = l2norm_prime(pred, Y) ;
  db2 = accu(G) * arma::ones<arma::mat>(1,1) ;
  dW2 = U.t() * G ;
  G = G * W2.t() ;
  G = sigmoid_prime(U) % G ;
  db1 = sum(G,0).t() ;
  dW1 = X.t() * G ;
}

// Update gradient
void upgrad (arma::mat &W1, arma::mat &W2,
             arma::mat &b1, arma::mat &b2,
             arma::mat const &dW1, arma::mat const &dW2,
             arma::mat const &db1, arma::mat const &db2,
             double learningrate) {
  W1 = W1 - learningrate * dW1 ;
  W2 = W2 - learningrate * dW2 ;
  b1 = b1 - learningrate * db1 ;
  b2 = b2 - learningrate * db2 ;
}


// Training neural network
// [[Rcpp::export]]
Rcpp::List nnetwork(arma::mat const &X, arma::mat const &Y,
                    arma::mat &W1, arma::mat &W2,
                    arma::mat &b1, arma::mat &b2,
                    int n_iter, double learningrate) {
  arma::mat U ;
  arma::mat pred ;
  arma::mat dW1 ;
  arma::mat dW2 ;
  arma::mat db1 ;
  arma::mat db2 ;
  for (int i =0; i<n_iter; ++i) {
    fprop(X, W1, W2, b1, b2, U, pred) ;
    bprop(X, Y, U, pred, W1, W2, b1, b2,
          dW1, dW2, db1, db2) ;
    upgrad(W1, W2, b1, b2, dW1, dW2, db1, db2, learningrate) ;
  }
  return Rcpp::List::create(Rcpp::Named("W1")=W1,
                            Rcpp::Named("W2")=W2,
                            Rcpp::Named("b1")=b1,
                            Rcpp::Named("b2")=b2);
}


// Predict neural network
arma::mat predict(arma::mat const &X, arma::mat const &W1,
                 arma::mat const &W2, arma::mat const &b1,
                 arma::mat const &b2) {
  arma::mat U ;
  arma::mat pred ;
  fprop(X, W1, W2, b1, b2, U, pred) ;
  return pred ;
}



