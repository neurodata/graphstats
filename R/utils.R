#' From Graph Helper
#'
#' function to standardize format internally for usage with n by n adjacency matrices
#' @param g an igraph object or an adjacency matrix.
#' @param edge.attr if g is an igraph object, the name of the edge attribute to use for weights.
#' @return An adjacency matrix corresponding to the graph.
#' @importFrom igraph as_adj
#' @keywords internal
#' @author Eric Bridgeford
gs.as_adj <- function(g, edge.attr=NULL) {
  if (class(g) == "igraph") {
    # construct an adjacency matrix from the initial igraph
    A <- as.matrix(as_adj(g, type="both", attr=edge.attr))
  } else {
    # not igraph object, so assume it's a matrix
    tryCatch({
      A <- as.matrix(g)
    }, error=function(e) stop("You have passed neither an 'igraph' object nor an object that can be cast to a symmetric 'matrix'."))
    dimA = dim(A)
    if (dimA[1] != dimA[2]) {
      stop("You have not passed a symmetric matrix.")
    }
  }
  return(A)
}

#' Output Helper
#'
#' Produces an output with the desired type.
#' @importFrom igraph graph_from_adjacency_matrix get.vertex.attribute set.vertex.attribute
#' @param A the n x n adjacency matrix.
#' @param gref a reference graph to use for adding vertex attributes.
#' @param edge.attr the edge attribute to (optionally) add weights from the adjacency matrix to.
#' @param output.type "matrix" or "graph".
#' @keywords internal
#' @author Eric Bridgeford
gs.output <- function(A, gref=NULL, edge.attr=NULL, output.type="matrix") {
  if (output.type == "graph") {
    if (class(gref) != "igraph") {
      gref=NULL
    }
    A <- gs.as_graph(A, gref=gref, edge.attr=edge.attr)
  }
  return(A)
}

#' To Graph Helper
#'
#' reconstruct a matrix from an adjacency matrix using vertex attributes retained from
#' a reference graph that was passed as input.
#' @importFrom igraph graph_from_adjacency_matrix get.vertex.attribute set.vertex.attribute
#' @param A the n x n adjacency matrix.
#' @param gref a reference graph to use for adding vertex attributes.
#' @param edge.attr the edge attribute to (optionally) add weights from the adjacency matrix to.
#' @return the graph as an igraph object.
#' @keywords internal
#' @author Eric Bridgeford
gs.as_graph <- function(A, gref=NULL, edge.attr=NULL) {
  ## IO specing
  # not igraph object, so assume it's a matrix
  tryCatch({
    A <- as.matrix(A)
  }, error=function(e) stop("You have not passed an object that can be cast as a 'matrix'."))
  dimA = dim(A)
  if (dimA[1] != dimA[2]) {
    stop("You have not passed a square matrix.")
  }

  ## meat and taters
  mode <- 'undirected'  # assume undirected
  if (!all(A == t(A))) {
    mode <- 'directed'  # it is a directed matrix if not symmetric
  }
  weighted <- FALSE  # assume unweighted
  if (!all(A %in% c(0, 1))) {
    if (!is.null(edge.attr)) {
      weighted <- edge.attr
    } else {
      weighted <- TRUE
    }
  }
  g <- graph_from_adjacency_matrix(A, mode=mode, weighted=weighted)
  if (!is.null(gref)) {
    for (vtx.attr in names(get.vertex.attribute(gref))) {
      # set the vertex attribute using the value in the previous graph
      g <- set.vertex.attribute(g, name=vtx.attr, index=V(g), value=get.vertex.attribute(gref, name=vtx.attr))
    }
  }
  return(g)
}
