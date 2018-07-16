#' Spectral Embedding of Adjacency Matrices
#'
#' Spectral decomposition of the adjacency matrices of graphs.
#'
#' This function computes a \code{k}-dimensional Euclidean representation of
#' the graph based on its adjacency matrix, \eqn{A}. This representation is
#' computed via the singular value decomposition of the adjacency matrix,
#' \eqn{A=UDV^T}.
#'
#' @importFrom igraph embed_adjacency_matrix
#' @importFrom irlba irlba
#' @param g an `\link[igraph]{igraph}` object or an `nxn` adjacency matrix with `n` vertices.
#' @param k The embedding dimensionality. Defaults to \code{NULL}. `k` should be less than the number of vertices in `g`. If \code{k==NULL}, defaults to \code{floor(log2(nvertices))}.}
#' @param edge.attr if `g` is a `\link[igraph]{igraph}`, the name of the attribute to use for weights. Defaults to \code{NULL}, which assumes the graph is binary.
#' \itemize{
#' \item{\code{is.null(edge.attr)} assumes unweighted.}
#' \item{\code{is.character(edge.attr)} assumes weighted with weights given by `edge.attr`.}
#' }
#' @return A list containing the following:
#' \item{\code{X}}{an \code{n} by \code{k} matrix indicating the estimated latent positions, where \code{n} is the number of vertices of \code{g}.}
#' \item{\code{Y}}{\code{NULL} if \code{g} is undirected. If \code{g} is directed, \code{Y} is a \code{n} by \code{k} matrix indicating the second half of the latent positions.}
#' \item{D}{The eigenvalues (for undirected graphs) or the singular values (for directed graphs) associated with the latent positions.}
#' @author Eric Bridgeford <ericwb95@@gmail.com>
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
gs.embed.ase <- function(g, k=NULL, edge.attr=NULL) {
  A <- gs.as_adj(g, edge.attr=edge.attr)
  return(gs.embed(A, k))
}

#' Spectral Embedding of the Laplacian of a Graph
#'
#' Spectral decomposition of Laplacian matrices of graphs.
#'
#' This function computes a \code{k}-dimensional Euclidean representation of
#' the graph based on its Laplacian matrix, \eqn{L}. This representation is
#' computed via the singular value decomposition of the Laplacian matrix.
#'
#' @importFrom igraph embed_adjacency_matrix
#' @param g an `\link[igraph]{igraph}` object or an `nxn` adjacency matrix with `n` vertices.
#' @param k The embedding dimensionality. Defaults to \code{NULL}. `k` should be less than the number of vertices in `g`. If \code{k==NULL}, defaults to \code{floor(log2(nvertices))}.}
#' @param edge.attr if `g` is a `\link[igraph]{igraph}`, the name of the attribute to use for weights. Defaults to \code{NULL}, which assumes the graph is binary.
#' \itemize{
#' \item{\code{is.null(edge.attr)} assumes unweighted.}
#' \item{\code{is.character(edge.attr)} assumes weighted with weights given by `edge.attr`.}
#' }
#' @return A list containing the following:
#' \item{\code{X}}{an \code{n} by \code{k} matrix indicating the estimated latent positions, where \code{n} is the number of vertices of \code{g}.}
#' \item{\code{Y}}{\code{NULL} if \code{g} is undirected. If \code{g} is directed, \code{Y} is a \code{n} by \code{k} matrix indicating the second half of the latent positions.}
#' \item{D}{The eigenvalues (for undirected graphs) or the singular values (for directed graphs) associated with the latent positions.}
#' @author Eric Bridgeford <ericwb95@@gmail.com>
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
#' TODO ebridge2
#' @export
gs.embed.lse <- function(g, k=NULL, edge.attr=NULL) {
  A <- gs.as_adj(g, edge.attr=edge.attr)
  return(gs.embed(A, k))
}

#' Embedding Helper
#'
#' A function to help with embedding points.
#' @param A the adjacency matrix to embed.
#' @param k the number of embedding dimensions.
#' @importFrom irlba irlba
#' @keywords internal
#' @author Eric Bridgeford <ericwb95@@gmail.com>
gs.embed <- function(A, k=NULL) {
  if (is.null(k)) {
    k <- floor(log2(dim(A)[1]))
  }
  if (length(k) > 1) { stop("The number of embedding dimensions 'k' has length > 1.") }
  if (class(k) != "numeric" && !is.integer(k)) { stop("The number of embedding dimensions 'k' is not a number.") }
  if (k%%1 != 0) { stop("The number of embedding dimensions 'k' must be an integer.") }
  if (k < 1) { stop("The number of embedding dimensions 'k' < 1.") }
  if (k > dim(A)[1]) { stop("The number of embedding dimensions 'k' is greater than number of vertices.") }

  nu <- k
  if (all(t(A) == A)) {
    nv <- 0
  } else {
    nv <- k
  }
  result <- irlba(A, nu=nu, nv=nv)
  if (nv != 0) {
    result$Y <- result$v;
  }
  result$X <- result$u; result$D <- result$d
  result$u <- NULL; result$v <- NULL; result$d <- NULL
  return(result)
}
