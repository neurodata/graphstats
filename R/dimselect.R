#' Dimensionality selection for singular values using profile likelihood.
#'
#' Select the number of significant singular values, by finding the
#' \sQuote{elbow} of the scree plot, in a principled way.
#'
#' The input of the function is a numeric vector which contains the measure of
#' \sQuote{importance} for each dimension.
#'
#' For spectral embedding, these are the singular values of the adjacency
#' matrix. The singular values are assumed to be generated from a Gaussian
#' mixture distribution with two components that have different means and same
#' variance. The dimensionality \eqn{d} is chosen to maximize the likelihood
#' when the \eqn{d} largest singular values are assigned to one component of
#' the mixture and the rest of the singular values assigned to the other
#' component.
#'
#' This function can also be used for the general separation problem, where we
#' assume that the left and the right of the vector are coming from two Normal
#' distributions, with different means, and we want to know their border. See
#' examples below.
#'
#' \code{dimselect} chooses how many coordinates to use in dimensionality reduction problems.
#' @param sdev A numeric vector, the ordered singular values.
#' @param n default value: 3; the number of returned elbows.
#' @param threshold either FALSE or a number. If threshold is a number, then all
#' the elements in d that are not larger than the threshold will be ignored.
#' @param plot logical. When T, it depicts a scree plot with highlighted elbows.
#' @return a vector of length n containing the positions of 'elbows'.
#' @author Youngser Park \email{youngser@@jhu.edu}, Gabor Csardi \email{csardi.gabor@@gmail.com}
#' @seealso \code{\link{embed_adjacency_matrix}}
#' @references M. Zhu, and A. Ghodsi (2006). Automatic dimensionality selection
#' from the scree plot via the use of profile likelihood. \emph{Computational
#' Statistics and Data Analysis}, Vol. 51, 918--930.
#' @keywords graphs
#' @examples
#'
#' # Generate the two groups of singular values with
#' # Gaussian mixture of two components that have different means
#' sing.vals  <- c( rnorm (10, mean=1, sd=1), rnorm(10, mean=3, sd=1) )
#' dim.chosen <- dimselect(sing.vals)
#' dim.chosen
#'
#' # Sample random vectors with multivariate normal distribution
#' # and normalize to unit length
#' lpvs <- matrix(rnorm(200), 10, 20)
#' lpvs <- apply(lpvs, 2, function(x) { (abs(x) / sqrt(sum(x^2))) })
#' RDP.graph  <- sample_dot_product(lpvs)
#' dimselect( embed_adjacency_matrix(RDP.graph, 10)$D )
#'
#' # Sample random vectors with the Dirichlet distribution
#' lpvs.dir    <- sample_dirichlet(n=20, rep(1, 10))
#' RDP.graph.2 <- sample_dot_product(lpvs.dir)
#' dimselect( embed_adjacency_matrix(RDP.graph.2, 10)$D )
#'
#' # Sample random vectors from hypersphere with radius 1.
#' lpvs.sph    <- sample_sphere_surface(dim=10, n=20, radius=1)
#' RDP.graph.3 <- sample_dot_product(lpvs.sph)
#' dimselect( embed_adjacency_matrix(RDP.graph.3, 10)$D )
#'
#' @export
dimselect <- function(sdev, n = 3, threshold = FALSE, plot = FALSE, main="") {

  if (class(sdev) != 'numeric' && (class(sdev) != 'matrix') && (class(sdev) != 'array')) {
    stop("Input object 'sdev' is not a one-dimensional numeric array.")
  }
  if (class(sdev) == 'matrix') {
    m = dim(sdev)[1]
    k = dim(sdev)[2]
    if (m > 1 && k > 1) {
      stop("Input object 'sdev' is not a one-dimensional numeric array.")
    }
  }
  if (sum("numeric" == apply(as.array(sdev),1,class)) != length(sdev)) { stop("Input object 'sdev' contains non-numeric values.") }
  d <- sort(sdev,decreasing=TRUE)
  if (!is.logical(threshold))
    d <- d[d > threshold]
  p <- length(d)
  if (p == 0) { stop(paste("Input object 'sdev' must have elements that are larger than the specified threshold value",
                           threshold), "!", sep="") }

  if (class(n) != "numeric") {
    stop("Input object 'n' is not a numeric value.")
  } else if (length(n) != 1) {
    stop("Input object 'n' is not a numeric value.")
  } else {
    if (n <= 0 || n >= length(d)) {
      stop("Input object 'n' must be in the appropriate interval.")
    }
  }

  if (!is.logical(plot)) { stop("Input object 'plot' is not a logical.") }

  lq <- rep(0.0, p)                     # log likelihood, function of q
  for (q in 1:p) {
    mu1 <- mean(d[1:q])
    mu2 <- mean(d[-(1:q)])              # = NaN when q = p
    sigma2 <- (sum((d[1:q] - mu1)^2) + sum((d[-(1:q)] - mu2)^2)) /
      (p - 1 - (q < p))
    lq[q] <- sum( dnorm(  d[1:q ], mu1, sqrt(sigma2), log=TRUE) ) +
      sum( dnorm(d[-(1:q)], mu2, sqrt(sigma2), log=TRUE) )
  }

  q <- which.max(lq)
  if (n > 1 && q < (p-1)) {
    q <- c(q, q + dimselect(d[(q+1):p], n-1, plot=FALSE))
  }

  if (plot==TRUE) {
    if (is.matrix(sdev)) {
      sdv <- d # apply(sdev,2,sd)
      plot(sdv,type="b",xlab="dim",ylab="stdev",main=main)
      points(q,sdv[q],col=2,pch=19)
    } else {
      plot(sdev, type="b",main=main)
      points(q,sdev[q],col=2,pch=19)
    }
  }

  return(q)
}
