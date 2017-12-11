#' Structured Independent Edge Model, Single Sample
#'
#' An independent-edge generalization of the traditional Stochastic Block Model.
#' @param X [v, v] the binary graphs with v vertices as a collection of adjacency matrices.
#' @param Es [[e]] a numbered list where each element corresponds to a community of edges. Each community should consist of the 1-dimensional indices of an edge.
#' @param
#' @return pr [e] a probability array corresponding to the probability of an edge connecting each edge community.
#' @return var [e] the variance of the probabilities estimated between edge communities.
#' @return dpr [e, e] an array consisting of the paired differences in probability where dpr_{ij} = pr_i - pr_j.
#' @return dvar [e, e] an array consisting of the variance of the estimate of the paired differences in probability, where dvar_{ij} = var_i + var_j.
#' @return p [e, e] p_{ij} is the p-value of false rejection of H0 that pr_i <= pr_j in favor of HA that pr_j > pr_i.
#' @author Eric Bridgeford
#' @export
gs.siem.fit <- function(X, Es) {
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
  p <- array(0, dim=c(nes, nes))
  dpr <- array(0, dim=c(nes, nes))
  dvar <- array(0, dim=c(nes, nes))

  for (i in 1:nes) {
    for (j in 1:nes) {
      p[i, j] <- model.ttest(pr[i], pr[j], vari[i], vari[j])
      dpr[i, j] <- pr[i] - pr[j]
      dvar[i, j] <- vari[i] + vari[j]
    }
  }

  return(list(pr=pr, var=vari))
}

#' 1 Sample Edge Test
#'
#' A function that computes the p value of falsely rejecting H0 that x_i !? x_j in favor of HA that x_j ? x_i for x_i, x_j from the same sample.
#' @param pr [e] an array of the edge probabilities.
#' @param var [e] the variance of the probabilities estimated between edge communities.
#' @param alt='greater' the alternative hypothesis for each edge test.
#' \itemize{
#' \item{'greater'}{p_{ij} corresponds to H0: pr_i <= pr_j, HA: pr_i > pr_j}
#' \item{'neq'}{p_{ij} corresponds to H0: pr_i = pr_j, HA: pr_i != pr_j}
#' \item{'less'}{p_{ij} corresponds to H0: pr_i >= pr_j, HA: pr_i < pr_j}
#' }
#' @return p [e, e] p_{ij} is the p-value of false rejection of H0 that pr_i !? pr_j in favor of HA that pr_j ? pr_i.
gs.siem.one_samp <- function(pr, vari, alt='greater') {
  nes <- length(pr)
  p <- array(0, dim=c(nes, nes))

  for (i in 1:nes) {
    for (j in 1:nes) {
      p[i, j] <- ana.est.ttest(pr[i], pr[j], vari[i], vari[j], 1, alt=alt)$p
    }
  }
  return(p)
}

#' 2 Sample Edge Test
#'
#' A function that computes the p value of falsely rejecting H0 that x_i !? x_j in favor of HA that x_j ? x_i for x_i, x_j from two different samples.
#' @param x1 [e] an array corresponding to an estimate on the edge probabilities for sample 1.
#' @param x2 [e] an array corresponding to an estimate on the edge probabilities for sample 2.
#' @param var1 [e] the variance of the preceding estimate.
#' @param var2 [e] the variance of the preceding estimate.
#' @param alt='greater' the alternative hypothesis for each edge test.
#' \itemize{
#' \item{'greater'}{p_{ij} corresponds to H0: pr_i <= pr_j, HA: pr_i > pr_j}
#' \item{'neq'}{p_{ij} corresponds to H0: pr_i = pr_j, HA: pr_i != pr_j}
#' \item{'less'}{p_{ij} corresponds to H0: pr_i >= pr_j, HA: pr_i < pr_j}
#' }
#' @return p [e, e] p_{ij} is the p-value of false rejection of H0 that pr_i !? pr_j in favor of HA that pr_j ? pr_i.
gs.siem.one_samp <- function(x1, x2, var1, var2, alt='greater') {
  nes <- length(pr)
  p <- array(0, dim=c(nes, nes))

  for (i in 1:nes) {
    p[i, j] <- ana.est.ttest(pr[i], pr[i], vari[i], vari[i], 2, alt=alt)$p
  }

  return(p)
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

#' Analytical T-Test for of an Estimator
#'
#' A function that computes an analytical t-test with 1 DOF for estimators.
#' @param e1 the first estimator.
#' @param e2 the second estimator.
#' @param var1 the variance of the first estimator.
#' @param var2 the variance of the second estimator.
#' @param df the number of degrees of freedom.
#' @param alt='greater' the alternative hypothesis for each edge test.
#' \itemize{
#' \item{'greater'}{p_{ij} corresponds to H0: pr_i <= pr_j, HA: pr_i > pr_j}
#' \item{'neq'}{p_{ij} corresponds to H0: pr_i = pr_j, HA: pr_i != pr_j}
#' \item{'less'}{p_{ij} corresponds to H0: pr_i >= pr_j, HA: pr_i < pr_j}
#' }
#' @return stat the test statistic.
#' @return p the p value associated with the test statistic
ana.est.ttest <- function(e1, e2, var1, var2, df, alt='greater') {
  num <- (e1 - e2)
  if (alt == 'less') {
    num <- -num
  } else if (alt == 'neq') {
    num <- abs(num)
  } else {
    stop("You have passed an invalid alternative hypothesis for the given test.")
  }
  tstat <- num/sqrt(var1 + var2)
  if (alt == 'neq') {
    p = 1 - 2*pt(tstat, df=df)
  } else {
    p = 1 - pt(tstat, df=df)
  }
  return(list(stat=tstat, p=p, df=df))
}
