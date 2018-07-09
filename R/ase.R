#' Spectral Embedding of Adjacency Matrices
#'
#' Spectral decomposition of the adjacency matrices of graphs.
#'
#' This function computes a \code{k}-dimensional Euclidean representation of
#' the graph based on its adjacency matrix, \eqn{A}. This representation is
#' computed via the singular value decomposition of the adjacency matrix,
#' \eqn{A=UDV^T}. In the case, where the graph is a random dot product graph
#' generated using latent position vectors in \eqn{R^{k}} for each vertex, the
#' embedding will provide an estimate of these latent vectors.
#'
#' For undirected graphs the latent positions are calculated as
#' \eqn{X=U^{k}D^{1/2}}{U[k] sqrt(D[k])}, where \eqn{U^{k}}{U[k]} equals
#' to the first \code{k} columns of \eqn{U}, and \eqn{D^{1/2}}{sqrt(D[k])} is
#' a diagonal matrix containing the top \code{k} singular values on the
#' diagonal.
#'
#' For directed graphs the embedding is defined as the pair
#' \eqn{X=U^{k}D^{1/2}}{U[k] sqrt(D[k])} and \eqn{Y=V^{k}D^{1/2}}{V[k]
#' sqrt(D[k])}. (For undirected graphs \eqn{U=V}, so it is enough to keep one
#' of them.)
#'
#' @param g The input graph, directed or undirected.
#' @param k An integer scalar. Should be the case that \code{k < gorder(g)}. The
#' largest \code{k}-dimensions are retained from the spectral embedding.
#' @param edge.attr the names of the attribute to use for weights. Should be in `names(get.edge.attribute(graph))`. Defaults to \code{NULL}, which assumes the graph is binary.
#' @return A list containing the following:
#' \item{\code{X}}{an \code{n} by \code{k} matrix indicating the estimated latent positions, where \code{n} is the number of vertices of \code{g}.}
#' \item{\code{Y}}{\code{NULL} if \code{g} is undirected. If \code{g} is directed, \code{Y} is a \code{n} by \code{k} matrix indicating the second half of the latent positions.}
#' \item{D}{The eigenvalues (for undirected graphs) or the singular values (for directed graphs) associated with the latent positions.}
#' \item{options}{A named list, information about the underlying ARPACK computation. See \code{\link{arpack}} for the details.}
#' @author Gabor Csardi \email{csardi.gabor@@gmail.com}
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
gs.embed.ase <- function(g, k, edge.attr=NULL) {

  # Input validation.
  if (class(g) == "dgCMatrix" || class(g) == "matrix") { g = graph_from_adjacency_matrix(g) }
  if (class(g) != 'igraph') { stop("Input object 'g' is not an igraph object.") }
  if (length(k) > 1) { stop("The number of embedding dimensions 'k' has length > 1.") }
  if (class(k) != "numeric" && !is.integer(k)) { stop("The number of embedding dimensions 'k' is not a number.") }
  if (k%%1 != 0) { stop("The number of embedding dimensions 'k' must be an integer.") }
  if (k < 1) { stop("The number of embedding dimensions 'k' < 1.") }
  if (k > gorder(g)) { stop("The number of embedding dimensions 'k' is greater than number of vertices.") }

  edge.attrs <- names(get.edge.attribute(g))
  if (!is.null(edge.attr)) {
    # remove attributes other than the one requested so the specified is used for ase
    if (!(edge.attr %in% edge.attrs)) {
      stop("You have not passed a valid 'edge.attr' for your graph.")
    }
    for (ea in edge.attrs[edge.attrs != edge.attr]) {
      g <- delete_edge_attr(g, ea)
    }
    wt <- get.edge.attribute(g, name=edge.attr)
    g <- set.edge.attribute(g, name="weight", value=wt)
    g <- delete_edge_attr(g, name=edge.attr)
  } else if (!is.null(edge.attrs)) {
    # user requests decomp of binary adjacency matrix
    for (ea in edge.attrs) {
      g <- delete_edge_attr(g, ea)
    }
  }

  # Produce matrix from igraph function.
  out <- embed_adjacency_matrix(g, k)
  return(out)
}
