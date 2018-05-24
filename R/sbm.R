#' Stochastic Block Model
#'
#' A function to fit a stochastic block model to a weighted or unweighted graph.
#'
#' @param g an \code{igraph} object. See \code{\link[igraph]{graph}} for details.
#' @param community.attribute the attribute of the graph vertices denoting the vertex communities. Should be that \code{community.attr %in% names(vertex.attributes(g))}.
#' @return the \code{[n, n]} probability matrix for each block, as an \code{igraph} object.
#' Vertices will be the unique communities for attribute \code{community.attribute}. Each vertex in the resulting graph will have an attribute \code{community} indicating the vertices in \code{graph} that
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
gs.sbm.fit <- function(g, community.attribute) {
  # get vertex names. If failure, just use 1:length(V(g))
  V.names <- get.vertex.attribute(g, name = "name")
  if (is.null(V.names)) {
    V.names <- 1:length(V(g))  # vertices are the numeric values of the vertices
  }
  # meat and taters of the algorithm
  g.mtx <- as_adjacency_matrix(g)  # convert to sparse adjacency matrix for simplicity
  V.comm <- get.vertex.attribute(g, community.attribute)
  un.comm <- unique(V.comm)
  P.mtx <- matrix(NaN, nrow=length(un.comm), ncol=length(un.comm))  # pre-allocate P for speed; P is dense so just use a regular matrix
  colnames(P.mtx) <- un.comm  # assign the vertex names for the new P matrix
  rownames(P.mtx) <- un.comm
  for (i in 1:length(un.comm)) {
    for (j in 1:length(un.comm)) {
      sg.ij <- g.mtx[V.comm == un.comm[i], V.comm == un.comm[j]]
      P.mtx[i, j] <- sum(sg.ij)/length(sg.ij)  # compute the average value within the subgraph i, j
    }
  }

  sbm.model <- graph_from_adjacency_matrix(P.mtx, weighted=TRUE)  # create a new matrix for P parameter of SBM
  # assign community attribute to each vertex indicating which vertices in the original graph comprise the new vertex in the SBM
  for (commi in un.comm) {
    sbm.model <- set_vertex_attr(sbm.model, "community", index=commi, list(vertices=V.names[which(V.comm == commi)]))
  }
  return(sbm.model)
}
