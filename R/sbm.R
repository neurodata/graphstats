#' Stochastic Block Model
#'
#' A function to fit the 3 parameter (number of vertices, community assignment for each vertex, and matrix community interactions)
#' Stochastic Block Model to a weighted or unweighted graph. The community interactions for pairs of unique communities is either a probability
#' matrix (unweighted graph) or an edge-expectation matrix (weighted graph).
#'
#' @param g an `\link[igraph]{igraph}` object or an `nxn` adjacency matrix with `n` vertices.
#' @param communities an `n` vector containing the community label for each of the `n` vertices in `g`.
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
#' @return An object of class `SBM` containing the following:
#' \describe{
#' \item{`n.v`}{the number of vertices in the graph.}
#' \item{`v.communities`}{the community each vertex is assigned to.}
#' \item{`P`}{a representation of the between-community edge probabilities (unweighted) or expectations (weighted).}
#'}
#' @author Eric Bridgeford
#' @export
gs.sbm.fit <- function(g, communities, edge.attr=NULL, output.type="matrix") {
  A <- gs.as_adj(g, edge.attr=edge.attr)

  un.comm <- unique(communities)
  P.mtx <- matrix(NaN, nrow=length(un.comm), ncol=length(un.comm))  # pre-allocate P for speed; P is dense so just use a regular matrix
  colnames(P.mtx) <- un.comm  # assign the vertex names for the new P matrix
  rownames(P.mtx) <- un.comm
  for (i in 1:length(un.comm)) {
    for (j in 1:length(un.comm)) {
      sg.ij <- A[communities == un.comm[i], communities == un.comm[j]]
      P.mtx[i, j] <- sum(sg.ij)/length(sg.ij)  # compute the average value within the subgraph i, j
    }
  }
  n.v <- dim(A)[1]
  rownames(P.mtx) <- un.comm; colnames(P.mtx) <- un.comm
  sbm.model <- gs.output(P.mtx, gref=NULL, edge.attr=edge.attr, output.type=output.type)
  out <- structure(list(n.v=n.v, v.communities=communities, P=sbm.model), class="SBM")
  return(out)
}
