#' Spectral Embedding of the Laplacian of a Graph
#'
#' Spectral decomposition of Laplacian matrices of graphs.
#'
#' This function computes a \code{no}-dimensional Euclidean representation of
#' the graph based on its Laplacian matrix, \eqn{L}. This representation is
#' computed via the singular value decomposition of the Laplacian matrix.
#'
#' They are essentially doing the same as \code{\link{embed_adjacency_matrix}},
#' but work on the Laplacian matrix, instead of the adjacency matrix.
#'
#' @param g The input graph, directed or undirected.
#' @param dim An integer scalar. This value is the embedding dimension of the
#' spectral embedding. Should be smaller than the number of vertices. The
#' largest \code{no}-dimensional non-zero singular values are used for the
#' spectral embedding.
#'
#' Possible values: \code{D-A} means \eqn{D-A} where \eqn{D} is the degree
#' matrix and \eqn{A} is the adjacency matrix; \code{DAD} means
#' \eqn{D^{1/2}}{D^1/2} times \eqn{A} times \eqn{D^{1/2}{D^1/2}},
#' \eqn{D^{1/2}}{D^1/2} is the inverse of the square root of the degree matrix;
#' \code{I-DAD} means \eqn{I-D^{1/2}}{I-D^1/2}, where \eqn{I} is the identity
#' matrix.  \code{OAP} is \eqn{O^{1/2}AP^{1/2}}{O^1/2 A P^1/2}, where
#' \eqn{O^{1/2}}{O^1/2} is the inverse of the square root of the out-degree
#' matrix and \eqn{P^{1/2}}{P^1/2} is the same for the in-degree matrix.
#'
#' \code{OAP} is not defined for undireted graphs, and is the only defined type
#' for directed graphs.
#'
#' The default (i.e. type \code{default}) is to use \code{D-A} for undirected
#' graphs and \code{OAP} for directed graphs.
#' @return A list containing with entries: \item{X}{Estimated latent positions,
#' an \code{n} times \code{dim} matrix, \code{n} is the number of vertices.}
#' \item{Y}{\code{NULL} for undirected graphs, the second half of the latent
#' positions for directed graphs, an \code{n} times \code{dim} matrix, \code{n}
#' is the number of vertices.} \item{D}{The eigenvalues (for undirected graphs)
#' or the singular values (for directed graphs) calculated by the algorithm.}
#' \item{options}{A named list, information about the underlying ARPACK
#' computation. See \code{\link{arpack}} for the details.}
#' @author Gabor Csardi \email{csardi.gabor@@gmail.com}
#' @seealso \code{\link{embed_adjacency_matrix}},
#' \code{\link{sample_dot_product}}
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
#' embed <- embed_laplacian_matrix(RDP, 5)
#' @export
lse <- function(g, dim) {

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
  X <- igraph::embed_laplacian_matrix(g, dim)$X
  return(X)
}
