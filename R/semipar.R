#'
#' Nonparametric two-sample testing using kernel-based test statistic
#'
#' This is a simple implementation of the kernel-based test statistic for the nonparametric
#' two-sample testing problem of given \eqn{X_1, X_2, \dots, X_n} i.i.d. \eqn{F} and
#' \eqn{Y_1, Y_2, \dots, Y_m} i.i.d. \eqn{G}, test the null hypothesis of \eqn{F = G} against
#' the alternative hypothesis of \eqn{F \not = G}. The test statistic is based on embedding
#' \eqn{F} and \eqn{G} into a reproducing kernel Hilbert space and then compute a distance between
#' the resulting embeddings. For this primitive, the Hilbert space is associated with the
#' Gaussian kernel.
#' @import mclust
#' @importFrom igraph gorder
#' @param g1 input graph, as an igraph object. See \code{\link[igraph]{graph}} for details.
#' @param g2 input graph, as an igraph object. See \code{\link[igraph]{graph}} for details.
#' @param k dimension of the latent position that graphs are embeded into.  Defaults to \code{NULL} which selects the optimal dimensionality using \link{gs.dim.select}.
#' @param nsamples Number of bootstrap samples when performing hypothesis tesing. Defaults to \code{2*n} where \code{n} is the number of vertices.
#' @param verbose logical indicating whether to print output to console. Defaults to \code{FALSE}.
#' @return a list containing the following:
#' \item{\code{T.obs}}{The observed test statistic for whether \code{g1} and \code{g2} are sampled from the same distribution.}
#' \item{\code{T.null}}{A length \code{nsamples} vector indicating the test statistic under the bootstrapped samples for whether \code{g1} and \code{g2} are sampled from the same distribution.}
#' \item{\code{p.value}}{the p-value associated with the semiparametric two-sample test.}
#' \code{T} A scalar value \eqn{T} such that \eqn{T} is near 0 if the rows of
#' \eqn{X} and \eqn{Y} are from the same distribution and \eqn{T} far from 0 if the rows of
#' \eqn{X} and \eqn{Y} are from different distribution.
#' @references Athreya, A., Fishkind, D. E., Levin, K., Lyzinski, V., Park, Y., Qin, Y., Sussman, D. L., Tang, M., Vogelstein, J. T., Priebe, C. E.
#' Statistical inference on random dot product graphs: a survey
#' @author Eric Bridgeford <ericwb95@@gmail.com>
#' @export
gs.test.semipar <- function(g1, g2, k=NULL, nsamples=2*max(gorder(g1), gorder(g2)), verbose=FALSE) {

}

select.dim <- function(g1, g2) {
  n1 = igraph::gorder(g1)
  n2 = igraph::gorder(g2)
  dim1_sbm <- gs.dim.select(g1, k=n1, n=1)$elbow[1]
  dim2_sbm <- gs.dim.select(g2, k=n2, n=1)$elbow[1]
  return(max(c(dim1_sbm, dim2_sbm)))
}
