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
#' @param g1 an igraph object
#' @param g2 an igraph object
#' @param dim dimension of the latent position that graphs are embeded into.  Defaults to \code{NULL} which selects the optimal dimensionality using \link{}.
#' @param sigma bandwidth of the rbf kernel for computing test statistic
#' @param alpha Significance level of hypothesis testing. Defaults to \code{.05}.
#' @param bootstrap_sample Number of bootstrap samples when performing hypothesis tesing
#' @param verbose logical indicating whether to print output to console. Defaults to \code{FALSE}.
#' @return \code{T} A scalar value \eqn{T} such that \eqn{T} is near 0 if the rows of
#' \eqn{X} and \eqn{Y} are from the same distribution and \eqn{T} far from 0 if the rows of
#' \eqn{X} and \eqn{Y} are from different distribution.
#' @references Tang, M., Athreya, A., Sussman, D.L., Lyzinski, V., Priebe, C.E.
#' A nonparametric two-sample hypothesis testing problem for random graphs
#' @author Eric Bridgeford <ericwb95@@gmail.com>, Youngser Park <youngser@@jhu.edu>, Kemeng Zhang <kzhang@@jhu.edu>.
#' @export
nonpar <- function(G1, G2, dim = NULL, sigma = NULL, alpha = 0.05, bootstrap_sample = 200, verbose = FALSE)
{
  # Check input format
  nonpar.validateInput(G1, G2, dim, sigma, alpha, bootstrap_sample, verbose)

  if (is.null(sigma)) {
    dim = select.dim(G1, G2)
  }
  Xhat1 = embed.graph(G1, dim)
  Xhat2 = embed.graph(G2, dim)
  if (is.null(sigma)) {
    sigma = get.sigma(Xhat1, Xhat2)
  }
  test_stat = test.stat(Xhat1, Xhat2, sigma)
  test_distribution = sampling.distribution(G1, G2, dim, bootstrap_sample)
  p_val = p_value(test_stat, test_distribution)
  num = round(bootstrap_sample * alpha)
  reject_threshold = test_distribution[num]
  reject = FALSE
  if (p_val <= alpha) {
    reject = TRUE
  }
  if (verbose) {
    if (reject) {
      print("Reject the nullhypothesis that two graphs are identically distributed.")
    } else {
      print("Fail to reject the null hypothesis that two graphs are identically distributed.")
    }
  }
  gg = plot.distribution(test_distribution, reject_threshold, test_stat)
  out = list(X1 = Xhat1, X2 = Xhat2, bandwidth = sigma, test_stats = test_stat,
             p_value = p_val, bootstrap_samples = test_distribution, plot = gg)
  return(out)
}

rect.dist <- function(X,Y) {
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  n <- nrow(X)
  m <- nrow(Y)
  tmp1 <- X%*%t(Y)
  tmp2 <- outer(rep(1, n), rowSums(Y^2))
  tmp3 <- outer(rowSums(X^2), rep(1,m))

  D <- tmp2 - 2*tmp1 + tmp3
  return(D)
}

embed.graph <- function(g, dim) {
  # Call ase to get latent position
  lpv = graphstats::ase(g, dim)
  # Fix signs of eigenvectors issue
  for (i in 1:dim) {
    if (sign(lpv[1, i]) != 1) {
      lpv[, i] = -lpv[, i]
    }
  }
  return(lpv)
}

select.dim <- function(G1, G2) {
  n1 = igraph::vcount(G1)
  n2 = igraph::vcount(G2)
  A1 = matrix(igraph::as_adj(G1), nrow = n1)
  A2 = matrix(igraph::as_adj(G2), nrow = n2)
  sigma1_sbm <- svd(A1, n1)$d
  sigma2_sbm <- svd(A2, n2)$d
  dim1_sbm <- dimselect(sigma1_sbm)[1]
  dim2_sbm <- dimselect(sigma2_sbm)[1]
  return(max(c(dim1_sbm, dim2_sbm)))
}

get.sigma <- function(X1, X2) {
  v1 = as.vector(stats::dist(X1))
  v2 = as.vector(stats::dist(X2))
  v = base::append(v1, v2)
  sigma = stats::median(v)
  return(sigma)
}

test.stat <- function(X, Y, sigma) {
  n <- nrow(X)
  m <- nrow(Y)
  tmpXX <- sum(exp(-(as.matrix(stats::dist(X))^2)/(2*sigma^2)))
  tmpYY <- sum(exp(-(as.matrix(stats::dist(Y))^2)/(2*sigma^2)))
  tmpXY <- sum(exp(-(rect.dist(X,Y))/(2*sigma^2)))
  tmp <- tmpXX/(n*(n-1)) + tmpYY/(m*(m-1)) - 2*tmpXY/(m*n)
  return((m+n)*tmp)
}


estimate.param <- function(G, model) {
  A = igraph::as_adjacency_matrix(G)
  A = as.matrix(A)
  predictions = max.col(model$z, 'first') - 1
  n = model$n
  K = model$G
  Rho = c()
  for (i in 1:K) {
    Rho[i] = sum(predictions == i - 1) / n
  }
  empty<- c()
  block_list = rep(list(empty),K)
  for (i in 1:n) {
    block = predictions[i] + 1
    block_list[[block]] = append(block_list[[block]], i)
  }

  Pmat <- matrix(rep(0, K*K), nrow = K)
  for (i in 2:K) {
    n_i_hat = length(block_list[[i]])
    for (j in 1:(i-1)) {
      n_j_hat = length(block_list[[j]])
      permute = expand.grid(block_list[[i]],block_list[[j]])
      p_row = nrow(permute)
      for (k in 1:p_row) {
        Pmat[i, j] = Pmat[i, j] + A[permute[k, 1],permute[k, 2]] / (n_i_hat * n_j_hat)
      }
      Pmat[j, i] = Pmat[i, j]
    }
  }

  for (i in 1:K) {
    n_i_hat = length(block_list[[i]])
    permute_diag = combn(block_list[[i]],2)
    permute_diag = t(permute_diag)
    pd_row = nrow(permute_diag)
    for (l in 1:pd_row) {
      Pmat[i, i] = Pmat[i, i] + A[permute_diag[l, 1],permute_diag[l, 2]] / (0.5 * n_i_hat * (n_i_hat-1))
    }
  }
  out = list(rho = Rho,P = Pmat)
  return(out)
}

sampling.distribution <- function(G1, G2, dim, bootstrap_sample_size) {
  Xhat1 = embed.graph(G1,dim)
  Xhat2 = embed.graph(G2,dim)

  model = mclust::Mclust(Xhat1,verbose = FALSE)
  n = model$n
  rho = estimate.param(G1, model)$rho
  P = estimate.param(G1, model)$P
  test_distribution = c()
  i = 1
  while (i <= bootstrap_sample_size) {
    tryCatch({
      G_a = igraph::sample_sbm(n, P, n * rho)
      G_b = igraph::sample_sbm(n, P, n * rho)
      Xhat_a = suppressWarnings(embed.graph(G_a, dim))
      Xhat_b = suppressWarnings(embed.graph(G_b, dim))
      sigma = get.sigma(Xhat_a, Xhat_b)
      ts = test.stat(Xhat_a, Xhat_b, sigma)
      test_distribution[i] = ts
      i = i + 1
    }, error=function(e) {NULL})
  }
  ordered = order(test_distribution,decreasing = TRUE)
  ts = test_distribution[ordered]
  return(ts)
}

p_value <- function(ts, test_distribution) {
  area = sum(test_distribution > ts) / length(test_distribution)
  return(area)
}

plot.distribution <- function(test_distribution, cv, ts) {
  m = length(test_distribution)
  minval = test_distribution[m]
  maxval = test_distribution[1]
  bw = (maxval - minval) / 30 # binwidth
  df = data.frame(1:m, test_distribution)
  colnames(df) = c('rank','test_statistics')
  s = seq(minval,maxval,length.out = 10)
  round.decimal <- function(x) sprintf("%.2f", x)
  q = ggplot2::ggplot(df, aes(test_statistics))
  q = q + ggplot2::geom_histogram(col="black",
                     fill="blue",
                     bins = 30,
                     alpha = 0.2) +
    ggplot2::labs(title="Histogram for Test Statistic") +
    ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
    ggplot2::labs(x="Bootstrapped Test Statistic", y="Frequency") +
    suppressWarnings(ggplot2::geom_density(color = "orange3", aes(num = m, binwidth = bw, y = ..density..*(num*binwidth)))) +
    ggplot2::scale_x_continuous(breaks = s, labels=round.decimal)
  q = q + geom_vline(aes(xintercept = cv, color = "Critical Value"), show.legend = TRUE)
  if (ts < maxval) {
    q = q + geom_vline(aes(xintercept = ts, color = "Test Statisic"), linetype = "dashed", show.legend = TRUE)
  }
  return(q)
}

nonpar.validateInput <- function(G1, G2, dim, sigma, alpha, bootstrap_sample, printResult) {

  if (class(G1) == "dgCMatrix") { G1 = igraph::graph_from_adjacency_matrix(G1) }
  if (class(G1) == "matrix") { G1 = igraph::graph_from_adjacency_matrix(G1) }
  if (class(G1) != 'igraph') { stop("Input object 'G1' is not an igraph object.") }
  if (class(G2) == "dgCMatrix") { G2 = igraph::graph_from_adjacency_matrix(G2) }
  if (class(G2) == "matrix") { G2 = igraph::graph_from_adjacency_matrix(G2) }
  if (class(G2) != 'igraph') { stop("Input object 'G2' is not an igraph object.") }
  if (!is.null(dim)) {
    if (class(dim) != "numeric" && !is.integer(dim)) { stop("Input 'dim' is not a number.") }
    if (dim%%1 != 0) { stop("Input 'dim' must be an integer.") }
    if (length(dim) > 1) { stop("Input 'dim' has length > 1.") }
    if (dim < 1) { stop("Number of dimensions 'dim' is less than 1.") }
    if (dim > igraph::gorder(G1) || dim > igraph::gorder(G2)) { stop("Num. Embedded dimensions 'dim' is greater than number of vertices.") }
  }

  if (!is.null(sigma)) {
    if (class(sigma) != "numeric") {
      stop("Input object 'sigma' is not a numeric value.")
    } else if (length(sigma) != 1) {
      stop("Input object 'sigma' is not a numeric value.")
    }
  }
  if (class(alpha) != "numeric") {
    stop("Input object 'alpha' is not a numeric value.")
  } else if (length(alpha) != 1) {
    stop("Input object 'alpha' is not a numeric value.")
  } else {
    if (alpha >= 1 || alpha <= 0) {
      stop("Significance level alpha must be strictly between 0 and 1.")
    }
  }
  if (class(bootstrap_sample) != "numeric") {
    stop("Input object 'bootstrap_sample' is not a numeric value.")
  } else if (length(bootstrap_sample) != 1) {
    stop("Input object 'bootstrap_sample' is not a numeric value.")
  } else {
    if (bootstrap_sample <= 20) {
      stop("The size of bootstrap sample is too small. Pick a larger value.")
    }
  }
  if (!is.logical(verbose)) { stop("Error: Input 'verbose' must be a logical.")}
}
