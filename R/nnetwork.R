#' @useDynLib nnetwork
#' @importFrom Rcpp evalCpp

#' @export
nnetwork <- function(x, ...) UseMethod("nnetwork")

#' @export
nnetwork.formula <- function(formula = formula(data),
                             data = list(),
                             ...)
{
  m <- match.call(expand.dots = FALSE)
  m$... <- NULL
  m[[1L]] <- quote(stats::model.frame)
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m)[,-1]
  y <- model.response(m)
  res <- nnetwork.default(x, y, ...)
  res$terms <- Terms
  res$call <- match.call()
  class(res) <- c("nnetwork.formula","nnetwork")
  return(res)
}

#' @export
nnetwork.default <- function(x, y, n.hidden, n.iter=1e+04,
                             learningrate= 0.01,
                             batch.size= nrow(x) , ...)
{
  x <- as.matrix(x)
  y <- as.matrix(y)
  if(any(is.na(x))) stop("missing values in 'x'")
  if(any(is.na(y))) stop("missing values in 'y'")
  if(dim(x)[1L] != dim(y)[1L]) stop("nrows of 'x' and 'y' must match")
  if(batch.size > nrow(x)) stop("can't take batch larger than data available")
  b1 <- as.matrix(rep(0, n.hidden))
  b2 <- as.matrix(mean(y))
  u1 <- 1 / sqrt(ncol(x) + n.hidden)
  u2 <- 1 / sqrt(n.hidden + 1)
  W1 <- matrix(runif(ncol(x)*n.hidden,-u1,u1), nrow=ncol(x))
  W2 <- matrix(runif(n.hidden,-u2,u2), nrow=n.hidden)
  net <- .Call("nnetwork_nnetwork", PACKAGE = "nnetwork", x, y, 
                          W1, W2, b1, b2, 
                          n.iter, learningrate, batch.size)
  net$call <- match.call()
  net$iter <- n.iter
  net$learningrate <- learningrate
  net$batchsize <- batch.size
  class(net) <- "nnetwork"
  net
}

#' @export
predict.nnetwork <- function(object, data, ...)
{
  if(!inherits(object, "nnetwork")) stop("object not of class \"nnetwork\"")
  if(inherits(object, "nnetwork.formula")) {
    data <- as.data.frame(data)
    ## work hard to predict NA for rows with missing data
    Terms <- delete.response(object$terms)
    m <- model.frame(Terms, data, na.action = na.omit)
    x <- as.matrix(model.matrix(Terms, m)[,-1])
  } else {
    x <- as.matrix(data)
    if(any(is.na(x))) stop("missing values in 'x'")
  }
  .Call("nnetwork_predict", PACKAGE = "nnetwork", x,
        object$W1, object$W2, object$b1, object$b2 )
}

#' @export
more<- function(x, ...) UseMethod("more")
#' @export
more.nnetwork <- function(object, data, n.iter, ...)
{
  if(!inherits(object, "nnetwork")) stop("object not of class \"nnetwork\"")
  if(inherits(object, "nnetwork.formula")) {
    data <- as.data.frame(data)
    ## work hard to predict NA for rows with missing data
    m <- model.frame(object$terms, data, na.action = na.omit)
    y <- as.matrix(model.response(m))
    Terms <- delete.response(object$terms)
    x <- as.matrix(model.matrix(Terms, m)[,-1])
  } else {
    stop("not implemented without formula object")
  }
  net <- .Call("nnetwork_nnetwork", PACKAGE = "nnetwork", x, y, 
               object$W1, object$W2, object$b1, object$b2, 
               n.iter, object$learningrate, object$batchsize)
  net$learningrate <- object$learningrate
  net$terms <- object$terms
  net$call <- object$call
  net$iter <- object$iter + n.iter
  net$batchsize <- object$batchsize
  class(net) <- c("nnetwork.formula","nnetwork")
  net
}






