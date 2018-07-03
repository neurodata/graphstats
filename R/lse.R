#' Spectral Embedding of the Laplacian of a Graph
#'
#' Spectral decomposition of Laplacian matrices of graphs.
#'
#' This function computes a \code{k}-dimensional Euclidean representation of
#' the graph based on its Laplacian matrix, \eqn{L}. This representation is
#' computed via the singular value decomposition of the Laplacian matrix.
#'
#' @importFrom igraph embed_adjacency_matrix gorder graph_from_adjacency_matrix
#' @param g The input graph, directed or undirected.
#' @param k An integer scalar. Should be the case that \code{k < gorder(g)}. The
#' largest \code{k}-dimensions are retained from the spectral embedding.
#' @return A list containing the following:
#' \item{\code{X}}{an \code{n} by \code{k} matrix indicating the estimated latent positions, where \code{n} is the number of vertices of \code{g}.}
#' \item{\code{Y}}{\code{NULL} if \code{g} is undirected. If \code{g} is directed, \code{Y} is a \code{n} by \code{k} matrix indicating the second half of the latent positions.}
#' \item{D}{The eigenvalues (for undirected graphs) or the singular values (for directed graphs) associated with the latent positions.}
#' \item{options}{A named list, information about the underlying ARPACK computation. See \code{\link{arpack}} for the details.}
#' @author Gabor Csardi \email{csardi.gabor@@gmail.com}
#' @seealso \code{\link{embed_adjacency_matrix}}, \code{\link{sample_dot_product}}
#' @references Sussman, D.L., Tang, M., Fishkind, D.E., Priebe, C.E.  A
#' Consistent Adjacency Spectral Embedding for Stochastic Blockmodel Graphs,
#' \emph{Journal of the American Statistical Association}, Vol. 107(499), 2012
#' @keywords graphs
#' @examples
#'
#' # A small graph
#' lpvs <- matrix(rnorm(200), 20, 10)
#' lpvs <- apply(lpvs, 2, function(x) { return (abs(x)/sqrt(sum(x^2))) })
#' RDP <- sample_dot_product(lpvs)
#' embed <- embed_laplacian_matrix(RDP, 5)
#' @export
gs.embed.lse <- function(g, k) {
  # Input validation.
  if (class(g) == "dgCMatrix" || class(g) == "matrix") { g = graph_from_adjacency_matrix(g) }
  if (class(g) != 'igraph') { stop("Input object 'g' is not an igraph object.") }
  if (length(k) > 1) { stop("The number of embedding dimensions 'k' has length > 1.") }
  if (class(k) != "numeric" && !is.integer(k)) { stop("The number of embedding dimensions 'k' is not a number.") }
  if (k%%1 != 0) { stop("The number of embedding dimensions 'k' must be an integer.") }
  if (k < 1) { stop("The number of embedding dimensions 'k' < 1.") }
  if (k > gorder(g)) { stop("The number of embedding dimensions 'k' is greater than number of vertices.") }

  # Produce matrix from igraph function.
  out <- embed_laplacian_matrix(g, k)
  return(out)
}
