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
#' @importFrom irlba irlba
#' \code{dimselect} chooses how many coordinates to use in dimensionality reduction problems.
#' @param X an object of class \link[igraph]{graph}, a numeric/complex matrix or 2-D array with \code{n} rows and \code{d} columns,
#' or a one-dimensional vector of class \code{numeric} containing ordered singular values.
#' If \code{X} is a \link[igraph]{graph}, the graph is embedded into \code{k} dimensions with \link[igraph]{embed_adjacency_matrix}. If
#' \code{X} is a \code{matrix} or 2-D \code{array}, the matrix is embedded into \code{k} dimensions with \link[base]{svd}.
#' \link[igraph]{embed_adjacency_matrix} if \code{X} is a \link[igraph]{graph} to obtain the singular values.
#' @param k If \code{X} is of class \link[igraph]{graph} or a numeric/complex matrix or 2-D array, an integer scalar indicating
#' the embedding dimensionality of the spectral embedding. Should have \code{k < length(V(X))} if \code{X} is of class
#' \link[igraph]{graph}, or \code{k < min(dim(X))} if \code{X} is a numeric/complex matrix or 2-D array.
#' @param n default value: 3; the number of returned elbows.
#' @param threshold either \code{FALSE} or an object of class \code{numeric}. If threshold is of class \code{numeric}, then all
#' the elements that are not larger than the threshold will be ignored.
#' @param plot logical. When \code{TRUE}, the return object includes a plot depicting the elbows.
#' @return list containing the following:
#' \item{\code{value}}{The singular values associated with each elbow in \code{elbow}.}
#' \item{\code{elbow}}{The indices of the elbows.}
#' \item{\code{plot}}{If \code{plot} is \code{TRUE}, contains a scree plot annotated with the elbows.}
#' @author Youngser Park \email{youngser@@jhu.edu}, Gabor Csardi \email{csardi.gabor@@gmail.com}, and Eric Bridgeford \email{ericwb95@@gmail.com}.
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
#' gs.dim.select( embed_adjacency_matrix(RDP.graph, 10)$D )
#'
#' # Sample random vectors with the Dirichlet distribution
#' lpvs.dir    <- sample_dirichlet(n=20, rep(1, 10))
#' RDP.graph.2 <- sample_dot_product(lpvs.dir)
#' gs.dim.select( embed_adjacency_matrix(RDP.graph.2, 10)$D )
#'
#' # Sample random vectors from hypersphere with radius 1.
#' lpvs.sph    <- sample_sphere_surface(dim=10, n=20, radius=1)
#' RDP.graph.3 <- sample_dot_product(lpvs.sph)
#' gs.dim.select( embed_adjacency_matrix(RDP.graph.3, 10)$D )
#'
#' @export
gs.dim.select <- function(X, k=NULL, n = 3, threshold = FALSE, plot = FALSE, main="") {

  if (class(X) == 'graph') {
    if (is.null(k)) {
      stop("Since 'X' is of class 'graph', you must specify 'k', the number of dimensions to embed into.")
    }
    d <- embed_adjacency_matrix(X, k)$D
  } else if (class(X) == 'matrix' || class(X) == 'array') {

    dim.X <- dim(X)
    if (length(dim.X) > 2) {
      stop("You have input an array with more than 2 dimensions.")
    }
    if (dim.X[1] > 1 && dim.X[2] > 1) {
      if (is.null(k)) {
        stop("Since 'X' is of class 'matrix' or 'array' and is 2-D, you must specify 'k', the number of dimensions to embed into.")
      }
      d <- irlba(X, nu=k)$d
    } else {
      tryCatch({
        d <- as.numeric(X)
      }, warning=function(w) {stop("Your input 'X' is a 1-D vector, array, or matrix, but has invalid entries and cannot be cast to numeric.")})
    }
  } else if (class(X) == 'numeric') {      tryCatch({
    d <- as.numeric(X)
  }, warning=function(w) {stop("Your input 'X' is a 1-D vector, array, or matrix, but has invalid entries and cannot be cast to numeric.")})
  } else {
    stop("Input object 'X' is not a graph, a matrix/complex matrix or 2-D array, array, nor a one-dimensional numeric array.")
  }

  if (sum("numeric" == apply(as.array(d),1,class)) != length(d)) { stop("Input object 'sdev' contains non-numeric values.") }
  d <- sort(d, decreasing=TRUE)
  if (is.numeric(threshold)) {
    d <- d[d > threshold]
    p <- length(d)
    if (p == 0) {
      stop(sprintf("The singular values do not have any elements larger than the threshold value, %f. The maximum singular value is %.3f.",
                   threshold, max(d)))
    }
  } else {
    p <- length(d)
  }

  if (class(n) != "numeric") {
    stop("Input object 'n' is not a numeric value.")
  } else if (length(n) != 1) {
    stop("Input object 'n' is not a numeric value.")
  } else {
    if (n <= 0 || n >= length(d)) {
      stop("Input object 'n' must be in the appropriate interval. n must be between 0 and %d, but n is %d.", length(d), n)
    }
  }

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
    q <- c(q, q + gs.dim.select(d[(q+1):p], n=n-1, plot=FALSE)$elbow)
  }

  if (!is.logical(plot)) { stop("Input object 'plot' is not a logical.") }

  out <- list(value=d[q], elbow=q)
  if (plot) {
    sv.obj <- data.frame(Dimension=1:p, Value=d)
    elbow.obj <- data.frame(Dimension=q, Value=d[q])
    plot <- ggplot(sv.obj, aes(x=Dimension, y=Value)) +
      geom_line(color='blue') +
      geom_point(data=elbow.obj, aes(x=Dimension, y=Value), color='red') +
      xlab("Dimension") +
      ylab("Singular Value")
    out$plot <- plot
  }

  return(out)
}
