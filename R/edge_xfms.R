#'
#' Run pass-to-rank on a weighted graph.
#'
#' It extracts a (non-zero) edge weight vector \eqn{W} from a graph and replaces it with \eqn{2*R / (|E|+1)} where \eqn{R} is the rank of \eqn{W} and \eqn{|E|} is the number of edges. This does 'no-op' for an unweighted or binary graph.
#'
#' @param g an `\link[igraph]{igraph}` object or an `nxn` adjacency matrix with `n` vertices.
#' @param edge.attr if `g` is a `\link[igraph]{igraph}`, the name of the attribute to use for weights. Defaults to `NULL`, which assumes the graph is binary.
#' \describe{
#' \item{`is.null(edge.attr)`}{constructs sbm on the graph as a binary adjacency matrix.}
#' \item{`is.character(edge.attr)`}{constructs sbm of the graph the graph as a weighted adjacency matrix, with edge-weights for `E(g)` given by `get.edge.attribute(g, attr=edge.attr)`.}
#' }
#' @param output.type the type of output to produce for the between community expectations. Defaults to `matrix`.
#' \describe{
#' \item{`"matrix"`}{produces a matrix for the between-communitity interactions.}
#' \item{`"graph"`}{produces an `\link[igraph]{igraph}` object for the between-community interactions.}
#' }
#' @return  an `\link[igraph]{igraph}` object or an `nxn` adjacency matrix with `n` vertices depending on `output.type`.
#' @author Eric Bridgeford <ericwb95@@gmail.com>
#' @export
gs.edge.ptr <- function(g, edge.attr=NULL, output.type="matrix") {
  A <- gs.as_adj(g, edge.attr=edge.attr)

  if (!all(unique(A) %in% c(0, 1))) {
    A.ptr <- A
    A.ptr[A != 0] <- rank(A[A != 0])*2 / (length(A) + 1)
  } else {
    # no-op
    A.ptr <- A
  }

  g <- gs.output(A.ptr, gref=g, edge.attr=edge.attr, output.type=output.type)
  return(g)
}

#'
#' Run pass-to-log on a weighted graph.
#'
#' Extracts edge weight vector \eqn{W} from a graph and replaces it with \eqn{log(W + eps)} where \eqn{log} is the logarithm operation.
#'
#' @param g an `\link[igraph]{igraph}` object or an `nxn` adjacency matrix with `n` vertices.
#' @param edge.attr if `g` is a `\link[igraph]{igraph}`, the name of the attribute to use for weights. Defaults to `NULL`, which assumes the graph is binary.
#' \describe{
#' \item{`is.null(edge.attr)`}{constructs sbm on the graph as a binary adjacency matrix.}
#' \item{`is.character(edge.attr)`}{constructs sbm of the graph the graph as a weighted adjacency matrix, with edge-weights for `E(g)` given by `get.edge.attribute(g, attr=edge.attr)`.}
#' }
#' @param base the base for the logarithm operation. Defaults to base `exp(1)`, or the natural logarithm. Options are real numbers greater than `0`.
#' @param eps the offset for taking the logarithm, in the event that there are entries of `g` that are less than or equal to `0`.
#' @param output.type the type of output to produce for the between community expectations. Defaults to `matrix`.
#' \describe{
#' \item{`"matrix"`}{produces a matrix for the between-communitity interactions.}
#' \item{`"graph"`}{produces an `\link[igraph]{igraph}` object for the between-community interactions.}
#' }
#' @return  an `\link[igraph]{igraph}` object or an `nxn` adjacency matrix with `n` vertices depending on `output.type`.
#' @author Eric Bridgeford <ericwb95@@gmail.com>
#' @export
gs.edge.log <- function(g, edge.attr=NULL, base=exp(1), eps=1, output.type="matrix") {
  A <- gs.as_adj(g, edge.attr=edge.attr)

  if (!all(unique(A) %in% c(0, 1))) {
    A.log <- log(A + eps, base=base)
  } else {
    # no-op
    A.log <- A
  }
  out <- gs.output(A.log, gref=g, edge.attr=edge.attr, output.type=output.type)
  return(out)
}

#'
#' Threshold a weighted graph.
#'
#' Extracts (non-zero) edge weight vector \eqn{W} from a graph and replaces it with \eqn{log(W + eps)} where \eqn{log} is the logarithm operation.
#'
#' @import igraph
#' @param g an `\link[igraph]{igraph}` object or an `nxn` adjacency matrix with `n` vertices.
#' @param edge.attr if `g` is a `\link[igraph]{igraph}`, the name of the attribute to use for weights. Defaults to `NULL`, which assumes the graph is binary.
#' \describe{
#' \item{`is.null(edge.attr)`}{constructs sbm on the graph as a binary adjacency matrix.}
#' \item{`is.character(edge.attr)`}{constructs sbm of the graph the graph as a weighted adjacency matrix, with edge-weights for `E(g)` given by `get.edge.attribute(g, attr=edge.attr)`.}
#' }
#' @param threshold the threshold for removing edges. Defaults to 0. Sets edges thresholded away to `0` and edges included in the threshold range to `1`.
#' @param method The method to use for thresholding. Defaults to `"absolute"`.
#' \itemize{
#' \item{"absolute"}{Remove edges less than `threshold`.}
#' \item{"percentile"}{Remove edges less than the `threshold` percentile. If `method` is `"percentile"`, `threshold` should be less than 1.`}
#' }
#' @param eps the offset for taking the logarithm, in the event that there are entries of `g` that are less than or equal to `0`.
#' @param output.type the type of output to produce for the between community expectations. Defaults to `matrix`.
#' \describe{
#' \item{`"matrix"`}{produces a matrix for the between-communitity interactions.}
#' \item{`"graph"`}{produces an `\link[igraph]{igraph}` object for the between-community interactions.}
#' }
#' @return  an `\link[igraph]{igraph}` object or an `nxn` adjacency matrix with `n` vertices depending on `output.type`.
#' @author Eric Bridgeford <ericwb95@@gmail.com>
#' @export
gs.edge.threshold <- function(g, edge.attr=NULL, threshold=0, method="absolute", output.type="matrix") {
  A <- gs.as_adj(g, edge.attr=edge.attr)

  if (!is.numeric(threshold)) {
    stop("You have not passed a numeric `threshold`.")
  }
  if (method == "percentile") {
    if (threshold > 1 || threshold < 0) {
      stop(sprintf("You have passed the threshold in an invalid range. Should be the percentile between 0 and 1. You passed: %.2f", threshold))
    }
    quant = quantile(as.numeric(A), threshold)
    A[A > quant] = 1; A[A < quant] = 0
  } else if (method == "absolute") {
    A[A > threshold] = 1; A[A < threshold] = 0
  } else {
    stop("You have passed an invalid option for `method`. Supported options are 'absolute' and 'percentile'.")
  }

  out <- gs.output(A, gref=g, edge.attr=edge.attr, output.type=output.type)
  return(g)
}
