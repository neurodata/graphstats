#' Spectral Embedding of Adjacency Matrices
#'
#' Spectral decomposition of the adjacency matrices of graphs.
#'
#' This function computes a \code{no}-dimensional Euclidean representation of
#' the graph based on its adjacency matrix, \eqn{A}. This representation is
#' computed via the singular value decomposition of the adjacency matrix,
#' \eqn{A=UDV^T}.In the case, where the graph is a random dot product graph
#' generated using latent position vectors in \eqn{R^{no}} for each vertex, the
#' embedding will provide an estimate of these latent vectors.
#'
#' For undirected graphs the latent positions are calculated as
#' \eqn{X=U^{no}D^{1/2}}{U[no] sqrt(D[no])}, where \eqn{U^{no}}{U[no]} equals
#' to the first \code{no} columns of \eqn{U}, and \eqn{D^{1/2}}{sqrt(D[no])} is
#' a diagonal matrix containing the top \code{no} singular values on the
#' diagonal.
#'
#' For directed graphs the embedding is defined as the pair
#' \eqn{X=U^{no}D^{1/2}}{U[no] sqrt(D[no])} and \eqn{Y=V^{no}D^{1/2}}{V[no]
#' sqrt(D[no])}. (For undirected graphs \eqn{U=V}, so it is enough to keep one
#' of them.)
#'
#' @param g The input graph, directed or undirected.
#' @param dim An integer scalar. This value is the embedding dimension of the
#' spectral embedding. Should be smaller than the number of vertices. The
#' largest \code{no}-dimensional non-zero singular values are used for the
#' spectral embedding.
#' @return A list containing with entries: \item{X}{Estimated latent positions,
#' an \code{n} times \code{no} matrix, \code{n} is the number of vertices.}
#' \item{Y}{\code{NULL} for undirected graphs, the second half of the latent
#' positions for directed graphs, an \code{n} times \code{no} matrix, \code{n}
#' is the number of vertices.} \item{D}{The eigenvalues (for undirected graphs)
#' or the singular values (for directed graphs) calculated by the algorithm.}
#' \item{options}{A named list, information about the underlying ARPACK
#' computation. See \code{\link{arpack}} for the details.}
#' @seealso \code{\link{sample_dot_product}}
#' @references Sussman, D.L., Tang, M., Fishkind, D.E., Priebe, C.E.  A
#' Consistent Adjacency Spectral Embedding for Stochastic Blockmodel Graphs,
#' \emph{Journal of the American Statistical Association}, Vol. 107(499), 2012
#' @keywords graphs
#' @examples
#'
#' ## A small graph
#' lpvs <- matrix(rnorm(200), 20, 10)
#' lpvs <- apply(lpvs, 2, function(x) { return (abs(x)/sqrt(sum(x^2))) })
#' RDP <- sample_dot_product(lpvs)
#' embed <- embed_adjacency_matrix(RDP, 5)
#' @export
ase <- function(g, dim) {

  # Input validation.
  if (class(g) == "dgCMatrix") { g = igraph::graph_from_adjacency_matrix(g) }
  if (class(g) == "matrix") { g = igraph::graph_from_adjacency_matrix(g) }
  if (class(g) != 'igraph') { stop("Input object 'g' is not an igraph object.") }
  if (length(dim) > 1) { stop("Input 'dim' has length > 1.") }
  if (class(dim) != "numeric" && !is.integer(dim)) { stop("Input 'dim' is not a number.") }
  if (dim%%1 != 0) { stop("Input 'dim' must be an integer.") }
  if (dim < 1) { stop("Number of dimensions 'dim' is less than 1.") }
  if (dim > igraph::gorder(g)) { stop("Num. Embedded dimensions 'dim' is greater than number of vertices.") }

  # Produce matrix from igraph function.
  X <- igraph::embed_adjacency_matrix(g, dim)$X
  return(X)
}
