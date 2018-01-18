#' Structured Independent Edge Model, Single Sample
#'
#' An independent-edge generalization of the traditional Stochastic Block Model.
#' @param X [v, v] the binary graph with v vertices.
#' @param Es [[e]] a numbered list where each element corresponds to a community of edges. Each community should consist of the 1-dimensional indices of an edge.
#' @param alt='greater' the alternative hypothesis for each edge test.
#' \itemize{
#' \item{'greater'}{p corresponds to H0: x1 <= x2, HA: x1 > x2. R(x1, x2) = x1 > x2}
#' \item{'neq'}{p corresponds to H0: x1 = x2, HA: x1 != x2. R(x1, x2) = x1 != x2}
#' \item{'less'}{p corresponds to H0: x1 >= x2, HA: x1 < x2. R(x1, x2) = x1 < x2}
#' }
#' @return pr [e] a probability array corresponding to the probability of an edge connecting each edge community.
#' @return var [e] the variance of the probabilities estimated between edge communities.
#' @return dpr [e, e] an array consisting of the paired differences in probability where dpr_{ij} = pr_i - pr_j.
#' @return dvar [e, e] an array consisting of the variance of the estimate of the paired differences in probability, where dvar_{ij} = var_i + var_j.
#' @return pv [e, e] pv_{ij} is the p-value of false rejection of H0 that !R(pr_i, pr_j) in favor of HA that R(pr_j, pr_i).
#' @author Eric Bridgeford
#' @export
gs.siem.fit <- function(X, Es, alt='greater') {
  # compute parameters for each edge-set
  params <- sapply(Es, function(e1) {
    gs.siem.model.params(X[e1])
  })

  # reorder for simplicity
  result <- list()
  pr <- unlist(params[1,])
  vari <- unlist(params[2,])

  nes <- length(Es)
  pv <- array(0, dim=c(nes, nes))
  dpr <- array(0, dim=c(nes, nes))
  dvar <- array(0, dim=c(nes, nes))

  for (i in 1:nes) {
    for (j in 1:nes) {
      pv[i, j] <- gs.siem.sample.test(pr[i], pr[j], vari[i], vari[j], df=1, alt=alt)$p
      dpr[i, j] <- pr[i] - pr[j]
      dvar[i, j] <- vari[i] + vari[j]
    }
  }

  return(list(pr=pr, var=vari, pv=pv, dpr=dpr, dvar=dvar))
}

#' Estimator Sample Test
#'
#' A function that computes the p value of falsely rejecting H0 that x_i !? x_j in favor of HA that x_j ? x_i for x_i, x_j from the same sample.
#' @param x1 expectation value of an estimator.
#' @param x2 expectation value of a second estimator.
#' @param var1 the variance of the first estimator.
#' @param var2 the variance of the second estimator.
#' @param df=NULL the number of degrees of freedom involved. Ignored if n1 and n2 are provided.
#' @param n1=NULL the number of observations for the first estimator. If n1 and n2 are not provided, the number of degrees of freedom is set to df.
#' @param n2=NULL the number of observations for the second estimator. If n1 and n2 are not provided, the number of degrees of freedom is set to df.
#' \itemize{
#' \item{1}{x1 and x2 are from different samples}
#' \item{2}{x1 and x2 are from different samples}
#' }
#' @param alt='greater' the alternative hypothesis for each edge test.
#' \itemize{
#' \item{'greater'}{p corresponds to H0: x1 <= x2, HA: x1 > x2. R(x1, x2) = x1 > x2}
#' \item{'neq'}{p corresponds to H0: x1 = x2, HA: x1 != x2. R(x1, x2) = x1 != x2}
#' \item{'less'}{p corresponds to H0: x1 >= x2, HA: x1 < x2. R(x1, x2) = x1 < x2}
#' }
#' @return p is the p-value of false rejection of H0 that !R(x1, x2) in favor of HA that R(x1, x2).
#' @export
gs.siem.sample.test <- function(x1, x2, var1, var2, df=NULL, n1=NULL, n2=NULL, alt='greater') {
  num <- (x1 - x2)
  if (is.null(df) & (is.null(n1) & is.null(n2))) {
    stop('Either df must be provided, or n1 and n2 should be provided.')
  }
  if (!is.null(df) & (!is.null(n1) | !is.null(n2))) {
    stop('If the df is set, neither n1 nor n2 should be provided.')
  } else if ((!is.null(n1) & !is.null(n2)) & !is.null(df)) {
    stop('If n1 and n2 are provided, df should not.')
  }
  if (alt == 'less') {
    num <- -num
  } else if (alt == 'neq') {
    num <- abs(num)
  } else if (alt != 'greater') {
    stop("You have passed an invalid alternative hypothesis for the given test.")
  }
  if (is.null(df)) {
    dfnum = (var1/n1 + var2/n2)^2
    dfdenom = var1^2/(n1^2*(n1 - 1)) + var2^4/(n2^2*(n2-1))
    df = round(dfnum/dfdenom)
    tstat = num/sqrt(var1/n1 + var2/n2)
  } else {
    tstat <- num/sqrt(var1 + var2)
  }
  if (alt == 'neq') {
    p = 1 - 2*pt(tstat, df=df)
  } else {
    p = 1 - pt(tstat, df=df)
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
gs.siem.model.params <- function(data) {
  n <- length(data)
  m.mu <- mean(data, na.rm=TRUE)
  m.var <- model.var(m.mu, n)
  return(list(p=m.mu, var=m.var))
}

is.defined = function(x)!is.null(x)

#' SIEM for Batch Detection
#'
#' A function that computes a test statistic associated with a particular arrangement of graphs
#' using pairs of edge community estimators estimated by the SIEM.
#' @import abind
#' @param models [[n]] a list of the models fit to each of the n samples.
#' @param Z [n] an array of the labels associated with each model.
#' @param i=1 the first edge set to use in the comparison.
#' @param j=2 the second edge set to use in the comparison.
#' @param tstat=gs.siem.batch.tstat.delta the test statistic to use. Should operate on a pair of models.
#' @param nperm=1000 the number of permutations for testing.
#' @return tstat.alt the test statistic given the current Z orderings.
#' @return tstat.null the test statistics of each permutation of the Z orderings.
#' @return P the estimator of E[tstat.null >= tstat.alt.]
#' @seealso \code{\link{gs.siem.fit}}
#' @author Eric Bridgeford
#' @export
gs.siem.batch.perm <- function(models, Z, i=1, j=2, tstat=gs.siem.batch.tstat, nperm=1000) {
  incl <- which(sapply(models, is.defined))
  Z <- Z[incl]
  models <- models[incl]
  tstat.alt <- do.call(tstat, list(models, Z, i=1, j=2))
  # compute the null distribution by permuting the observed labels
  tstat.nulls <- lapply(1:nperm, function(k) {
    permuted_ss <- sample(Z, size=length(Z))
    null <- do.call(tstat, list(models, permuted_ss, i=i, j=j))  # get test statistic of permuted data
    return(null)
  })
  # compare the test statistic of the observed data to the alternative statistics
  cmp <- lapply(tstat.nulls, function(null) {
    tstat.alt <= null
  })
  # compute E[tstat.null > tstat.alt]
  P <- colMeans(abind::abind(cmp, along=0), dims=1) + 1/nperm
  return(list(tstat.alt=tstat.alt, tstat.nulls=tstat.nulls, P=P))
}

#' Test Statistic for Batch Detection
#'
#' A function that computes a test statistic associated with a set of estimators.
#' Given a list of SIEM models and an array of labels, computes the mean estimator for
#' two pairs of the average edge-set estimators (called p and q). For each pair of unique labels
#' in the data set, computes the distance between the pairs of estimators assiciated with the unique labels as
#' D_{ij} = |p_i - p_j - (q_i - q_j)|.
#' @param models [[n]] a list of the models fit to each of the n samples.
#' @param Z [n] an array of the labels associated with each model.
#' @param i=1 the first edge set to use in the comparison.
#' @param j=2 the second edge set to use in the comparison.
#' @param return="full" How to return the test statistic.
#' \itemize{
#' \item{"full"}{return the full distance matrix as the statistic.}
#' \item{"max"}{return the max value of the distance matrix.}
#' \item{"mean"}{return the mean value of the distance matrix.}
#' \item{"min"}{return the min value of the distance matrix.}
#' }
#' @return stat the test statistic.
#' @author Eric Bridgeford
#' @export
gs.siem.batch.tstat <- function(models, Z, i=1, j=2, return="full") {
  p <- sapply(models, function(model) model$pr[i])
  q <- sapply(models, function(model) model$pr[j])
  # compute the average p and q associated with each unique Z label
  Zset <- unique(Z)
  n <- length(Zset)
  pset <- sapply(Zset, function(z) mean(p[Z == z], na.rm=TRUE))
  qset <- sapply(Zset, function(z) mean(q[Z == z], na.rm=TRUE))
  # for each pair of unique labels, compute the distance between the estimators
  D <- array(NaN, dim=c(n, n))
  for (i in 1:n) {
    for (j in i:n) {
      D[i, j] <- abs(pset[i] - pset[j] - (qset[i] - qset[j]))
    }
  }
  # return the statistic depending on how the user specifies
  if (return == "full") {
    stat <-  D
  } else if (return == "max") {
    stat <- max(D)
  } else if (return == "mean") {
    stat <- mean(D)
  } else if (return == "min") {
    stat  <- min(D)
  }
  return(stat)
}
