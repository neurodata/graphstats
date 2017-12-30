#' Cross-Validation Data Splitter
#'
#' \code{sg.bern.xval_split_data} A function to split a series of graphs into
#' training and testing sets for cross validation.
#'
#' @param samp [v, v, s] an array of input data.
#' @param Y [s] the class labels.
#' @param k='loo' the cross-validated method to perform.
#' \itemize{
#' \item{'loo'}{Leave-one-out cross validation}
#' \item{isinteger(k)}{perform k-fold cross-validation with k as the number of folds.}
#' }
#' @return sets the cross-validation sets.
#' @export
#'
gs.xval.split <- function(samp, Y, k='loo') {
  Y <- factor(Y)
  n <- length(Y)
  if (k == 'loo') {
    k <- n  # loo is just xval with k=n
  }
  if (round(k) == k) {  # then xval is an integer
    samp.ids <- as.matrix(sample(1:n, n))  # the sample ids randomly permuted
    k.folds <- split(samp.ids, rep(1:k), drop=TRUE)  # split the sample ids into xval folds

    sets <- sapply(k.folds, function(fold) {
      list(X.train=X[-fold,,,drop=FALSE], Y.train=Y[-fold,drop=FALSE],
           X.test=X[fold,,,drop=FALSE], Y.test=Y[fold,drop=FALSE])
      })
  } else {
    stop("You have not entered a valid parameter for xval.")
  }
  return(sets)
}
