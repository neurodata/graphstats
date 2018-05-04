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
#'
#' @param G1 an igraph object
#' @param G2 an igraph object
#' @param dim dimension of the latent position that graphs are embeded into
#'
#' @return \code{T} A scalar value \eqn{T} such that \eqn{T} is near 0 if the rows of
#' \eqn{X} and \eqn{Y} are from the same distribution and \eqn{T} far from 0 if the rows of
#' \eqn{X} and \eqn{Y} are from different distribution.
#'
#' @author Youngser Park <youngser@jhu.edu>
#' @export

nonpar <- function(G1, G2, dim, sigma = NULL, alpha = 0.05, bootstrap_sample = 200, plot = FALSE)
{
  # Check input format
  if (class(G1) != 'igraph') { stop("Input object 'G1' is not an igraph object.") }
  if (class(G2) != 'igraph') { stop("Input object 'G2' is not an igraph object.") }
  if (class(dim) != "numeric") {
    stop("Input object 'dim' is not a numeric value.")
  } else if (length(dim) != 1) {
    stop("Input object 'dim' is not a numeric value.")
  } else {
    if (dim <= 0) {
      stop("Input object 'dim' must be positive.")
    }
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
  if (reject) {
    print("Reject null")
  } else {
    print("Fail to reject null")
  }
  if (plot == TRUE) {
    hist(test_distribution)
    abline(v=test_stat,col="blue")
    abline(v=reject_threshold,col = 'red')
  }
  out = list(X1 = Xhat1, X2 = Xhat2, bandwidth = sigma, test_stats = test_stat,
             p_value = p_val, t_d = test_distribution)
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
  for (i in 1:dim) {
    if (sign(lpv[1, i]) != 1) {
      lpv[, i] = -lpv[, i]
    }
  }
  return(lpv)
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

  model = mclust::Mclust(Xhat1)
  n = model$n
  rho = estimate.param(G1, model)$rho
  P = estimate.param(G1, model)$P
  test_distribution = c()

  for (i in 1:bootstrap_sample_size) {
    G_a = igraph::sample_sbm(n,P, n * rho)
    G_b = igraph::sample_sbm(n,P, n * rho)
    Xhat_a = embed.graph(G_a,dim)
    Xhat_b = embed.graph(G_b,dim)
    sigma = get.sigma(Xhat_a, Xhat_b)
    ts = test.stat(Xhat_a, Xhat_b, sigma)
    test_distribution[i] = ts
  }
  ordered = order(test_distribution,decreasing = TRUE)
  ts = test_distribution[ordered]
  return(ts)
}

p_value <- function(ts, test_distribution) {
  area = sum(test_distribution > ts) / length(test_distribution)
  return(area)
}






