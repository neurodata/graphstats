#' Cross-Validation Data Splitter
#'
#' \code{gs.xval.split} A function to split a dataset into
#' training and testing sets for cross validation.
#'
#' @param graphs \code{[n, d, d]} or \code{[[n]][v, v]} the data with \code{n} samples of \code{v} vertices.
#' @param Y \code{[n]} the labels of the samples with \code{K} unique labels.
#' @param k the cross-validated method to perform. Defaults to \code{'loo'}.
#' \itemize{
#' \item \code{'loo'}  Leave-one-out cross validation
#' \item \code{isinteger(k)}  perform \code{k}-fold cross-validation with \code{k} as the number of folds.
#' }
#' @param ... trailing args.
#' @return sets the cross-validation sets as an object of class \code{"XV"}. Each element of the list contains the following items:
#' \item{\code{graphs.train}}{the training data as a \code{[[n - k]][v, v]} list.}
#' \item{\code{Y.train}}{the training labels as a \code{[n - k]} vector.}
#' \item{\code{graphs.test}}{the testing data as a \code{[[k]][v, v]} list.}
#' \item{\code{Y.test}}{the testing labels as a \code{[k]} vector.}
#' @author Eric Bridgeford
#' @examples
#' # prepare data for 10-fold validation
#' library(graphstats)
#' data <- gs.sims.er(n=100, v=10, priors=c(1), p=c(0.5))  # 100 examples of 10x10 graphs
#' graphs <- data$graphs; Y <- data$Y
#' sets.xval.10fold <- lol.xval.split(graphs, Y, k=10)
#'
#' # prepare data for loo validation
#' sets.xval.loo <- lol.xval.split(graphs, Y, k='loo')
#'
#' @export
gs.xval.split <- function(graphs, Y, k='loo', ...) {
  Y <- factor(Y)
  n <- length(Y)
  if (k == 'loo') {
    k <- n  # loo is just xval with k=n
  }
  if (round(k) == k) {  # then xval is an integer
    samp.ids <- as.matrix(sample(1:n, n))  # the sample ids randomly permuted
    k.folds <- split(samp.ids, rep(1:k), drop=TRUE)  # split the sample ids into xval folds
    # partition X and Y appropriately into training and testing sets
    sets <- lapply(k.folds, function(fold) {
      list(graphs.train=graphs[-fold], Y.train=Y[-fold,drop=FALSE],
           graphs.test=graphs[fold], Y.test=Y[fold,drop=FALSE])
    })
  } else {
    stop("You have not entered a valid parameter for xval.")
  }
  return(structure(sets), class="XV")
}
