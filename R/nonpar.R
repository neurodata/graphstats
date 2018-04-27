#'
#' Nonparametric two-sample testing using kernel-based test statistic
#'
#' This is a simple implementation of the kernel-based test statistic for the nonparametric
#' two-sample testing problem of given \eqn{X_1, X_2, \dots, X_n} i.i.d. \eqn{F} and
#' \eqn{Y_1, Y_2, \dots, Y_m} i.i.d. \eqn{G}, test the null hypothesis of \eqn{F = G} against
#' the alternative hypothesis of \eqn{F \not = G}. The test statistic is based on embedding
#' \eqn{F} and \eqn{G} into a reproducing kernel Hilbert space and then compute a distance between
#' the resulting embeddings. For this primitive, the Hilbert space is associated with the
#' Gaussian kernel.
#'
#' @param Xhat1 an \eqn{n} x \eqn{d} matrix
#' @param Xhat2 an \eqn{n} x \eqn{d} matrix
#' @param sigma a bandwidth for the Gaussian kernel
#'
#' @return \code{T} A scalar value \eqn{T} such that \eqn{T} is near 0 if the rows of
#' \eqn{X} and \eqn{Y} are from the same distribution and \eqn{T} far from 0 if the rows of
#' \eqn{X} and \eqn{Y} are from different distribution.
#'
#' @author Youngser Park <youngser@jhu.edu>
#' @export

nonpar <- function(Xhat1,Xhat2,sigma=0.5)
{
  # Check input format
  if (class(Xhat1) != 'matrix') { stop("Input object 'Xhat1' is not a matrix.") }
  if (class(Xhat2) != 'matrix') { stop("Input object 'Xhat2' is not a matrix.") }
  if (class(sigma) != "numeric") {
    stop("Input object 'sigma' is not a numeric value.")
  } else if (length(sigma) != 1) {
    stop("Input object 'sigma' is not a numeric value.")
  } else {
    if (sigma <= 0) {
      stop("Input object 'sigma' must be positive.")
    }
  }

  n <- nrow(Xhat1)
  m <- nrow(Xhat2)

  tmpXX <- sum(exp(-(as.matrix(stats::dist(Xhat1))^2)/(2*sigma^2)))
  tmpYY <- sum(exp(-(as.matrix(stats::dist(Xhat2))^2)/(2*sigma^2)))
  tmpXY <- sum(exp(-(rect.dist(Xhat1,Xhat2))/(2*sigma^2)))

  tmp <- tmpXX/(n*(n-1)) + tmpYY/(m*(m-1)) - 2*tmpXY/(m*n)

  return((m+n)*tmp)
}


rect.dist <- function(X,Y){
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  n <- nrow(X)
  m <- nrow(Y)
  tmp1 <- X%*%t(Y)
  tmp2 <- outer(rep(1, n), rowSums(Y^2))
  tmp3 <- outer(rowSums(X^2), rep(1,m))

  D <- tmp2 - 2*tmp1 + tmp3
  return(D)
}
