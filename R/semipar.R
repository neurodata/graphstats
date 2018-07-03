#'
#' Semiparametric two-sample testing
#'
#' This is a simple implementation of the semiparametric
#' two-sample testing problem of given \eqn{X_1, X_2, \dots, X_n} i.i.d. \eqn{F} and
#' \eqn{Y_1, Y_2, \dots, Y_m} i.i.d. \eqn{G}, test the null hypothesis of \eqn{F = G} against
#' the alternative hypothesis of \eqn{F \not = G}.
#' @import mclust
#' @importFrom igraph gorder
#' @param g1 input graph, as an igraph object. See \code{\link[igraph]{graph}} for details.
#' @param g2 input graph, as an igraph object. See \code{\link[igraph]{graph}} for details.
#' @param k dimension of the latent position that graphs are embeded into.  Defaults to \code{NULL} which selects the optimal dimensionality using \link{gs.dim.select}.
#' @param nsamples Number of bootstrap samples when performing hypothesis tesing. Defaults to \code{2*n} where \code{n} is the number of vertices, \code{max(gorder(g1), gorder(g2))}.
#' @param test the type of test statistic to use. Defaults to \code{'R'}. Supported options are:
#' \itemize{
#' \item{\code{'R'}}{estimate only a rotation between the latent positions of \code{g1} and \code{g2} when computing the test statistic.}
#' }
#' @param verbose logical indicating whether to print output to console. Defaults to \code{FALSE}.
#' @return a list containing the following:
#' \item{\code{T.obs}}{The observed test statistic for whether \code{g1} and \code{g2} are sampled from the same distribution. If \code{T.obs} is near 0, this indicates that \code{g1} and \code{g2} likely come from the same distribution.}
#' \item{\code{T.null}}{A length \code{nsamples} vector indicating the test statistic under the bootstrapped samples for whether \code{g1} and \code{g2} are sampled from the same distribution.}
#' \item{\code{p.value}}{the p-value associated with the semiparametric two-sample test; the fraction of times \code{T.alt < T.obs}.}
#' @references Athreya, A., Fishkind, D. E., Levin, K., Lyzinski, V., Park, Y., Qin, Y., Sussman, D. L., Tang, M., Vogelstein, J. T., Priebe, C. E.
#' Statistical inference on random dot product graphs: a survey
#' @author Eric Bridgeford <ericwb95@@gmail.com>
#' @export
gs.test.semipar <- function(g1, g2, k=NULL, nsamples=2*max(gorder(g1), gorder(g2)), test='R', verbose=FALSE) {
  # if k is unspecified, estimate using Ghodsi et al.
  if (is.null(k)) {
    k <- select.dim(g1, g2)
  }
  # compute alternative from the observed data directly using latent positions of g1 and g2 embedded
  # into X1hat and X2hat
  X1hat <- gs.embed.ase(g1, k)$X
  X2hat <- gs.embed.ase(g2, k)$X
  T.obs <- tstat.semipar.lp(X1hat, X2hat, test)

  # use bootstrapping approach to compute the null
  T.null <- sapply(1:nsamples, function(samp) {
    # sample 2 graphs from latent positions of X1hat and X2hat respectively; that is,
    # G1.i ~ Bern(Xihat*Xihat^T); G2.i ~ Bern(Xihat*Xihat^T)
    # given null that F=G, it should be the case that under the null we would expect
    # T.obs to be similar to max(tstat(G1.i, G2.i)) over i
    tstat.null <- sapply(list(X1hat, X2hat), function(Xhat) {
      g1.samp <- gs.sims.sample_latent(Xhat)
      g2.samp <- gs.sims.sample_latent(Xhat)
      # compute alternative from parametrically bootstrapped data using g1 and g2 sampled
      # from Xhat
      tnull <- tstat.semipar.graph(g1.samp, g2.samp, k, test)
      return(tnull)
    })
    return(max(tstat.null))
  })

  p.value <- mean(T.obs < T.null)
  return(list(T.obs=T.obs, T.null=T.null, p.value=p.value))
}

select.dim <- function(g1, g2) {
  n1 = igraph::gorder(g1)
  n2 = igraph::gorder(g2)
  dim1_sbm <- gs.dim.select(g1, k=n1, n=1)$elbow[1]
  dim2_sbm <- gs.dim.select(g2, k=n2, n=1)$elbow[1]
  return(max(c(dim1_sbm, dim2_sbm)))
}

# a function for semipar from the graphs themselves for cleanness of above code
tstat.semipar.graph <- function(g1, g2, k, test) {
  X1hat <- gs.embed.ase(g1, k)$X
  X2hat <- gs.embed.ase(g2, k)$X
  tstat <- tstat.semipar.lp(X1hat, X2hat, test)
  return(tstat)
}
# a function for semipar from the latent positions
tstat.semipar.lp <- function(X1, X2, test) {
  if (test == 'R') {
    # frobenius norm btwn X1hat and X2hat rotated optimally to align with X1hat
    tstat <- norm(X1 - procrustes(X1, X2)$rotation %*% X2, type="F")
  }
  return(tstat)
}
