#' Zero-Inflated Poisson Model
#'
#' A function for fitting the 3-parameter (number of vertices, number of non-zero edges, lambda poisson rate parameter) Zero-Inflated Poisson Model to a graph.
#' A poisson model is fitted to the non-zero edges.
#'
#' @param g an `\link[igraph]{igraph}` object or an `nxn` adjacency matrix with `n` vertices.
#' @param communities an `n` vector containing the community label for each of the `n` vertices in `g`.
#' @param edge.attr if `g` is a `\link[igraph]{igraph}`, the name of the attribute to use for weights. Defaults to `NULL`, which assumes the graph is binary.
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
#' @return An object of class `ZIP` containing the following:
#' \describe{
#' \item{`n.v`}{the number of vertices in `g`.}
#' \item{`ne.nz`}{the number of non-zero edges in `g`.}
#' \item{`lambda`}{the poisson rate parameter for non-zero edges in `g`; also, the average non-zero edge weight of `g`.}
#'}
#' @author Eric Bridgeford
#' @export
gs.poisson.fit.zip <- function(g, edge.attr=NULL, output="matrix") {
  A <- gs.as_adj(g, edge.attr=edge.attr)
  n.v <- dim(A)[1]
  ne.nz <- sum(A != 0)
  lambda <- mean(A[A != 0])
  out <- structure(list(n=n.v, ne.nz=ne.nz, lambda=lambda), class="ZIP")
  return(out)
}
