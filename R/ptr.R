#'
#' Run pass-to-rank on a weighted graph.
#'
#' It extracts (non-zero) edge weight vector \eqn{W} from a graph and replaces it with \eqn{2*R / (|E|+1)} where \eqn{R} is the rank of \eqn{W} and \eqn{|E|} is the number of edges. This does 'no-op' for an unweighted graph.
#'
#' @param g a graph in \code{igraph} format or an n x 2 edge list or an n x n adjacency matrix
#'
#' @author Youngser Park <youngser@jhu.edu>
#' @export
#' @import igraph
ptr <- function(g)
{
  if (class(g) != "igraph") {
    if (!is.matrix(g)) stop("the input has to be either an igraph object or a matrix!")
    else {
      if (ncol(g)==2) g <- graph_from_edgelist(g)
      else if (nrow(g)==ncol(g)) g <- graph_from_adjacency_matrix(g, weighted = TRUE)
      else stop("the input matrix is not a graph format!")
    }
  }

  if (is.weighted(g)) {
    W <- E(g)$weight
  } else { # no-op!
    W <- rep(1,ecount(g))
  }

  E(g)$weight <- rank(W)*2 / (ecount(g)+1)
  return(g)
}
