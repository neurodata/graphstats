#' Omnibus
#'
#' A function for creating an omnibus graph from arbitrarily many input graphs with
#' matched vertex sets.
#'
#' Given \eqn{A_1, A_2, ..., A_m} a collection of (possibly weighted) adjacency matrices of a collection
#' of \eqn{m} undirected graphs with matched vertices. Then the \eqn{mn} by \eqn{mn} Omnibus Matrix has the
#' subgraph where \eqn{M_{ij} = 1/2 (A_i + A_j)}.
#'
#' @param A1 the first \code{[n, n]} matrix.
#' @param A2 the second \code{[n, n]} matrix.
#' @return the omnibus matrix, as an object of class \link[igraph]{igraph}. The vertices are concatenated to form one
#' @author Youngser Park
#' @export
gs.omni <- function(A1, A2) {
  A11 <- A1
  A22 <- A2
  A12 <- (A1 + A2)/2
  A21 <- (A2 + A1)/2
  # Take these four matrices and put them in an omnibus matrix,
  # with layout [ A11 A12 ]
  #		[ A21 A22 ]
  if( (nrow(A11) != nrow(A12))
      | (nrow(A21) != nrow(A22))
      | (ncol(A11) != ncol(A21))
      | (ncol(A12) != ncol(A22)) ) {
    stop('Matrix dimensions incompatible for forming omnibus matrix.');
  }
  nn <- nrow(A11)

  M <- matrix( nrow=2*nn, ncol=2*nn )
  M[1:nn, 1:nn] <- A11
  M[(nn+1):(2*nn), (nn+1):(2*nn)] <- A22
  M[1:nn, (nn+1):(2*nn)] <- A12
  M[(nn+1):(2*nn), 1:nn] <- A21

  return(M)
}
