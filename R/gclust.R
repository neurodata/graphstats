#' Select number of mixture components for Gaussian Mixture Model via maximizing BIC.
#'
#' \code{glust} selects optimal number of mixture components to describe data in GMM.
#'
#' @param X A matrix object with n rows (data points) by d columns (dimensions).
#' @param K=2 The maximum number of components to be considered. Model selected from 1 to K components.
#' @param verbose=FALSE whether to print Mclust clustering output.
#' @return The number of components that maximizes BIC.
#' @importFrom mclust mclustBIC
#' @export
gclust <- function(X, K=2, verbose = FALSE) {

  # Input validation
  if (!is.matrix(X) || !is.numeric(X)) { stop("Input 'X' is not a numeric matrix.")}
  if (!is.numeric(K) || K%%1 != 0) { stop("Input 'K' must be an integer.") }
  if (length(K) > 1) { stop("Input 'K' must be an integer.") }
  if (K < 1) { stop("Input 'K' must be greater than or equal to 1.") }

  # Fit model and retrieve optimal cluster number.
  return(mclust::Mclust(X, 1:K, verbose=verbose)$G)
}
