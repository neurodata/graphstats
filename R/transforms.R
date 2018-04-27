#' Apply Graph Model
#'
#' A function to streamline applying a model over a set of graphs.
#'
#' @importFrom plyr alply
#' @param graphs the set of \code{n} graphs with \code{v} vertices. Can be either:
#' \itemize{
#' \item{\code{[[n]][v, v]}}{a list of \code{n} graphs with \code{v} vertices.}
#' \item{\code{[n, v, v]}}{an array of \code{n} graphs with \code{v} vertices.}
#' }
#' @param model.func the model function to apply to each of the n graphs. Should take as its first argument a \code{[v, v]} graph adjacency matrix, and return a model object.
#' @param model.opts the hyper-parameter options to pass to your model as a keyworded list. Defaults to \code{list()}, or no hyper-parameters.
#' @param ... trailing args.
#' @return models \code{[[n]]} list where \code{models[[i]]} corresponds to the model \code{model.func} applied to \code{graphs[[i]]}.
#' @author Eric Bridgeford
#' @export
gs.xfm.apply_model <- function(graphs, func, model.opts=list(), ...) {
  if (is.array(graphs)) {
    if (length(graphs) == 3) {
      if (dim(graphs)[2] == dim(graphs)[3]) {
        graphs <- alply(list_in, 3)
      } else {
        stop("Your graphs are not [v, v].")
      }
    } else {
      stop("Your graphs do not have 3 indices, [n, v, v].")
    }
  } else if (!is.list(graphs)) {
    stop("You have passed an invalid object for graphs. Should be an [n, v, v] array, or [[n]][v, v] list.")
  }
  return(lapply(graphs, function(graph) {
    do.call(func, c(list(graph), model.opts))
  }))
}

#' Rank Graph
#'
#' A function to rank the edges of a graph.
#' @param graph \code{[v, v]} a graph with \code{v} vertices.
#' @param ties the method to break ties. Defaults to \code{'average'}.
#' \itemize{
#' \item{'average'}{take the average rank between the tied items.}
#' \item{'first'}{choose the lowest index as the tie breaker.}
#' \item{'last'}{choose the highest index as the tie breaker.}
#' \item{'random'}{randomly break ties.}
#' \item{'max'}{replace all tied values with the maximum rank they could occupy.}
#' \item{'min'}{replace all tied values with the minimum rank they could occupy.}
#' }
#' @param na.last How to interpret missing values in the data. Defaults to \code{TRUE}.
#' \itemize{
#' \item{TRUE}{missing values in the data are put last}
#' \item{FALSE}{missing values in the data are put first}
#' }
#' @param ... trailing args.
#' @return R \code{[v, v]} the rank of each edge, from lowest (1) to highest (n^2, or otherwise depending on ties).
#' @author Eric Bridgeford
#' @seealso \code{\link{stats::rank}}
#' @export
gs.xfm.rank_graph <- function(graph, ties='average', na.last=TRUE, ...) {
  R <- matrix(rank(graph, ties.method=ties, na.last=na.last), ncol=ncol(graph))
  return(R)
}

#' Threshold Graph
#'
#' A function to threshold and binarize the edges of a graph.
#' @importFrom stats quantile
#' @param graph \code{[v, v]} a graph with v vertices.
#' @param thresh the threshold to use for binarization.
#' @param method the method to threshold. Defaults to \code{'quantile'}.
#' \itemize{
#' \item{'quantile'}{use the \code{thresh*100} percentile of edge values to threshold, setting values under this to 0 and over to 1.}
#' \item{'abs'}{use the threshold value provided explicitly to binarize, setting values under thresh to 0 and over to 1.}
#' }
#' @param ... trailing args.
#' @return R \code{[v, v]} the rank of each edge, from lowest (\code{1}) to highest (\code{v^2}, or otherwise depending on ties).
#' @author Eric Bridgeford
#' @export
gs.xfm.thresh_graph <- function(graph, thresh=0.5, method='quantile', ...) {
  thr = quantile(graph, thresh)
  R <- ifelse(graph > thr, 1, 0)
  return(R)
}
