#' Stochastic Block Model
#'
#' A function to fit a stochastic block model to a weighted or unweighted graph.
#'
#' @param g an \code{igraph} object. See \code{\link[igraph]{graph}} for details.
#' @param communities a \code{[[n]]} list indicating the community assignments of vertices in \code{graph}. That is, all vertices in \code{communities} are in \code{V(graph)},
#' all vertices in \code{V(graph)} are assigned a community, and no vertex in \code{V(graph)} is assigned to multiple communities.
#' @return the \code{[n, n]} probability matrix for each block, as an \code{igraph} object.
#' Vertices will be the \code{names(communities)}. Each vertex in the resulting graph will have an attribute \code{community} indicating the vertices in \code{graph} that
#' comprise the particular community the vertex summarizes See \code{\link[igraph]{graph}} for details.
#' @examples
#' library(graphstats)
#'
#' # sample a graph from an SBM
#' g <- sample_sbm(n=20, block.sizes=c(12, 8), pref.matrix=cbind(c(0.8, 0.2), c(0.2, 0.8)))
#' communities <- list(a=V(g)[1:12], b=V(g)[13:20])  # define communities of vertices in g
#' sbm.g <- gs.sbm.fit(g, communities)  # fit SBM to graph
#' @author Eric Bridgeford
#' @export
gs.sbm.fit <- function(g, communities) {
  V.communities <- do.call(c, communities)  # get the union of vertices indicated in communities

  # basic checks
  # all vertices in communities are in V(graph)
  if (!all(V.communities %in% V(g))) {
    stop("communities is not a disjoint subset of V(graph). Vertices in communities exist that do not exist in V(graph).")
  }
  # all vertices in V(graph) are assigned a community
  if (any(!(V(g) %in% V.communities))) {
    stop("communities is not a disjoint subset of V(graph). Vertices in V(graph) are not assigned to a community.")
  }
  if (length(unique(V.communities)) != length(V.communities)) {
    stop("communities is not a disjoint subset of V(graph). There exists a vertex(ices) assigned to multiple communities.")
  }

  # meat and taters of the algorithm
  g.mtx <- as_adjacency_matrix(g)  # convert to sparse adjacency matrix for simplicity
  V.P <- names(communities)  # the vertex names for P are the names of the communities
  P.mtx <- matrix(NaN, nrow=length(V.P), ncol=length(V.P))  # pre-allocate P for speed; P is dense so just use a regular matrix
  colnames(P.mtx) <- V.P  # assign the vertex names for the new P matrix
  rownames(P.mtx) <- V.P
  for (i in 1:length(communities)) {
    for (j in 1:length(communities)) {
      sg.ij <- g.mtx[communities[[V.P[i]]], communities[[V.P[j]]]]
      P.mtx[i, j] <- sum(sg.ij)/length(sg.ij)  # compute the average value within the subgraph i, j
    }
  }

  sbm.model <- graph_from_adjacency_matrix(P.mtx, weighted=TRUE)  # create a new matrix for P parameter of SBM
  # assign community attribute to each vertex indicating which vertices in the original graph comprise the new vertex in the SBM
  for (V.Pi in V.P) {
    sbm.model <- set_vertex_attr(sbm.model, "community", index=V.Pi, list(vertices=communities[[V.Pi]]))
  }
  return(sbm.model)
}
