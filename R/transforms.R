#' Apply Over array of graphs and return array of graphs
#'
#' A function to streamline applying a function over a set of graphs as an array and producing an array.
#' @import abind
#' @param Xs [n, v, v] the set of n graphs with v vertices in an array.
#' @param func the function to apply to each of the n graphs. Should take as its first argument a [v, v] matrix, and return ONLY a [v, v] matrix.
#' @param rparam=NULL  if func does not return a [v, v] matrix but has one particular parameter that is, specify it here.
#' @param ... the additional arguments that would otherwise be passed to func, ordered or keyworded as necessary.
#' @return Rs [n, v, v] the set of n graphs wit v vertices, transformed according to func
#' @author Eric Bridgeford
#' @export
gs.xfm.aaply <- function(Xs, func, rparam=NULL, ...) {
  n <- dim(Xs)[1]
  if (is.null(rparam)) {
    Rs <- sapply(1:n, function(i) {
      do.call(func, list(Xs[i,,], ...))
    }, simplify=FALSE)
  } else {
    Rs <- sapply(1:n, function(i) {
      do.call(func, list(Xs[i,,], ...))[[rparam]]
    }, simplify=FALSE)
  }
  Rs <- do.call(abind::abind, c(Rs, list(along=0)))
  return(Rs)
}

#' Apply Over array of graphs and return list per graph
#'
#' A function to streamline applying a function over a set of graphs as an array and a list of function fits to each graph.
#' @import abind
#' @param Xs [n, v, v] the set of n graphs with v vertices in an array.
#' @param func the function to apply to each of the n graphs. Should take as its first argument a [v, v] matrix.
#' @param ... the additional arguments that would otherwise be passed to func, ordered or keyworded as necessary.
#' @return Rs [[n]] the returns of func applied to each [v, v] entry of Xs.
#' @author Eric Bridgeford
#' @export
gs.xfm.alply <- function(Xs, func, ...) {
  n <- dim(Xs)[1]
  Rs <- sapply(1:n, function(i) {
    do.call(func, c(list(Xs[i,,]), list(...)))
  }, simplify=FALSE)
  return(Rs)
}

#' Rank Graph
#'
#' A function to rank the edges of a graph.
#' @param X [v, v] a graph with v vertices.
#' @param ties='average' the method to break ties.
#' \itemize{
#' \item{'average'}{take the average rank between the tied items.}
#' \item{'first'}{choose the lowest index as the tie breaker.}
#' \item{'last'}{choose the highest index as the tie breaker.}
#' \item{'random'}{randomly break ties.}
#' \item{'max'}{replace all tied values with the maximum rank they could occupy.}
#' \item{'min'}{replace all tied values with the minimum rank they could occupy.}
#' }
#' @param na.last=TRUE
#' \itemize{
#' \item{TRUE}{missing values in the data are put last}
#' \item{FALSE}{missing values in the data are put first}
#' }
#' @return R [v, v] the rank of each edge, from lowest (1) to highest (n^2, or otherwise depending on ties).
#' @author Eric Bridgeford
#' @seealso \code{\link{stats::rank}}
#' @export
gs.xfm.rank_graph <- function(X, ties='average', na.last=TRUE) {
  R <- matrix(rank(X, ties.method=ties, na.last=na.last), ncol=ncol(X))
  return(R)
}

#' Threshold Graph
#'
#' A function to threshold and binarize the edges of a graph.
#' @import stats
#' @param X [v, v] a graph with v vertices.
#' @param thresh the threshold to use for binarization.
#' @param method='quantile' the method to threshold.
#' \itemize{
#' \item{'quantile'}{use the thresh*100 percentile of edge values to threshold, setting values under this to 0 and over to 1.}
#' \item{'abs'}{use the threshold value provided explicitly to binarize, setting values under thresh to 0 and over to 1.}
#' }
#' @return R [v, v] the rank of each edge, from lowest (1) to highest (n^2, or otherwise depending on ties).
#' @author Eric Bridgeford
#' @export
gs.xfm.thresh_graph <- function(matrix, thresh=0.5, method='quantile') {
  thr = stats::quantile(matrix, thresh)
  R <- ifelse(matrix > thr, 1, 0)
  return(R)
}
