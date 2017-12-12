#' Structured Independent Edge Model, Single Sample
#'
#' An independent-edge generalization of the traditional Stochastic Block Model.
#' @param X [v, v] the binary graphs with v vertices as a collection of adjacency matrices.
#' @param Es [[e]] a numbered list where each element corresponds to a community of edges. Each community should consist of the 1-dimensional indices of an edge.
#' @param alt='greater' the alternative hypothesis for the p-value.
#' @return pr [e] a probability array corresponding to the probability of an edge connecting each edge community.
#' @return var [e] the variance of the probabilities estimated between edge communities.
#' @return dpr [e, e] an array consisting of the paired differences in probability where dpr_{ij} = pr_i - pr_j.
#' @return dvar [e, e] an array consisting of the variance of the estimate of the paired differences in probability, where dvar_{ij} = var_i + var_j.
#' @return pv [e, e] pv_{ij} is the p-value of false rejection of H0 that pr_i !? pr_j in favor of HA that pr_j ? pr_i.
#' @author Eric Bridgeford
#' @export
gs.siem.fit <- function(X, Es, alt='greater') {
  X[X > 0] <- 1  # ensure binarization

  # compute parameters for each edge-set
  params <- sapply(Es, function(e1) {
    model.params(X[e1])
  })

  # reorder for simplicity
  result <- list()
  pr <- unlist(params[1,])
  vari <- unlist(params[2,])

  nes <- length(pr)
  pv <- array(0, dim=c(nes, nes))
  dpr <- array(0, dim=c(nes, nes))
  dvar <- array(0, dim=c(nes, nes))

  for (i in 1:nes) {
    for (j in 1:nes) {
      pv[i, j] <- gs.siem.sample_test(pr[i], pr[j], vari[i], vari[j], 1, alt=alt)$p
      dpr[i, j] <- pr[i] - pr[j]
      dvar[i, j] <- vari[i] + vari[j]
    }
  }

  return(list(pr=pr, var=vari, pv=pv, dpr=dpr, dvar=dvar))
}

#' Estimator Sample Test
#'
#' A function that computes the p value of falsely rejecting H0 that x_i !? x_j in favor of HA that x_j ? x_i for x_i, x_j from the same sample.
#' @param x1 value of an estimator.
#' @param x2 value of a second estimator.
#' @param var1 the variance of the first estimator.
#' @param var2 the variance of the second estimator.
#' @param nsamp the number of samples involved. If x1 and x2 are from the same sample, nsamp is 1.
#' \itemize{
#' \item{1}{x1 and x2 are from different samples}
#' \item{2}{x1 and x2 are from different samples}
#' }
#' @param alt='greater' the alternative hypothesis for each edge test.
#' \itemize{
#' \item{'greater'}{p corresponds to H0: x1 <= x2, HA: x1 > x2}
#' \item{'neq'}{p corresponds to H0: x1 = x2, HA: x1 != x2}
#' \item{'less'}{p corresponds to H0: x1 >= x2, HA: x1 < x2}
#' }
#' @return p is the p-value of false rejection of H0 that x1 !? x2 in favor of HA that x1 ? x2.
#' @export
gs.siem.sample_test <- function(x1, x2, var1, var2, nsamp, alt='greater') {
  num <- (x1 - x2)
  if (alt == 'less') {
    num <- -num
  } else if (alt == 'neq') {
    num <- abs(num)
  } else if (alt != 'greater') {
    stop("You have passed an invalid alternative hypothesis for the given test.")
  }
  tstat <- num/sqrt(var1 + var2)
  if (alt == 'neq') {
    p = 1 - 2*pt(tstat, df=nsamp)
  } else {
    p = 1 - pt(tstat, df=nsamp)
  }
  return(list(stat=tstat, p=p, df=df))
}

#' SIEM Model Variance
#'
#' A function that computes the variance per-edge for SIEM.
#' @param p the probability of a particular edge existing for an edge community.
#' @param n the number of edges for this particular edge community.
#' @return var the variance associated with a particular edge community.
#' @author Eric Bridgeford
model.var <- function(p, n) {
  p*(1 - p)/n
}

#' SIEM Model Parameters
#'
#' A function that computes the parameters per-edge for SIEM.
#' @param data an array containing the edge data for a particular edge in a single graph.
#' @return var the variance associated with a particular edge community.
#' @author Eric Bridgeford
model.params <- function(data) {
  n <- length(data)
  m.mu <- sum(data)/n
  m.var <- model.var(m.mu, n)
  return(list(p=m.mu, var=m.var))
}
