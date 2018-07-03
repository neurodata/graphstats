#' Stochastic Block Model
#'
#' A function to fit a stochastic block model to a weighted or unweighted graph.
#'
#' @param g an \code{igraph} object. See \code{\link[igraph]{graph}} for details.
#' @param community.attribute the attribute of the graph vertices denoting the vertex communities. Should be that \code{community.attribute %in% names(vertex.attributes(g))}.
#' @param edge.attr the names of the attribute to use for weights. Should be in `names(get.edge.attribute(graph))`. Defaults to \code{NULL}, which assumes the graph is binary.
#' \itemize{
#' \item{\code{is.null(edge.attr)} constructs sbm on the graph as a binary adjacency matrix.}
#' \item{\code{is.character(edge.attr)} constructs sbm of the graph the graph as a weighted adjacency matrix, with edge-weights for \code{E(g)} given by \code{E(g)[[edge.attr]]}.}
#' }
#' @param vertex.label an attribute for naming the vertices. Defaults to \code{NULL}.
#' \itemize{
#' \item{\code{vertex.label==FALSE} name the vertices \code{V(g)} sequentially, as 1, 2, ... n.}
#' \item{\code{vertex.label==TRUE} name the vertices \code{V(g)} as \code{V(g)$name}.}
#' }
#' @return the probability matrix for each block, as an \code{igraph} object.
#' Vertices will be the unique communities for attribute \code{community.attribute}. Each vertex in the resulting graph will have an attribute \code{community} indicating the vertices in \code{graph} that
#' comprise the particular community the vertex summarizes See \code{\link[igraph]{graph}} for details.
#'
#' @author Eric Bridgeford
#' @export
gs.sbm.fit <- function(g, community.attribute, edge.attr=NULL, vertex.label=FALSE) {
  # get vertex names. If failure, just use 1:length(V(g))
  V.names <- get.vertex.attribute(g, "name")
  if (is.null(V.names)) {
    V.names <- 1:length(V(g))  # vertices are the numeric values of the vertices
  }
  # meat and taters of the algorithm

  g.mtx <- as_adjacency_matrix(g, attr=edge.attr, names=vertex.label, type="both", sparse=FALSE)  # convert to sparse adjacency matrix for simplicity
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
  if (!is.null(edge.attr) && edge.attr != "weight") {
    sbm.model <- set_edge_attr(sbm.model, name=edge.attr, index=E(sbm.model), as.numeric(E(sbm.model)$weight))
    sbm.model <- delete_edge_attr(sbm.model, "weight")
  }
  # assign community attribute to each vertex indicating which vertices in the original graph comprise the new vertex in the SBM
  for (commi in un.comm) {
    sbm.model <- set_vertex_attr(sbm.model, "community", index=commi, list(vertices=V.names[which(V.comm == commi)]))
  }
  return(sbm.model)
}
