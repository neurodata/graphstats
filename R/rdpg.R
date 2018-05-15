#' Samples a pair of correlated \eqn{G(X)} random dot product graphs
#'
#' @param X \eqn{n \times d} random matrix
#' @param rho Numeric scalar in the unit interval, the target Pearson correlation between
#' the adjacency matrieces of the original and the generated graph.
#'
#' @return A list of two igraph objects, named \code{A} and \code{B}, which are
#' two graphs whose adjacency matrix entries are correlated with \code{rho}.
#'
#' @author Minh Tang <scent.of.time@gmail.com>
#' @export
rdpg.sample.correlated <- function(X, rho) {

  P <- X %*% t(X)
  n <-  nrow(P)
  U <- matrix(0, nrow = n, ncol = n)
  U[col(U) > row(U)] <- runif(n*(n-1)/2)
  U <- (U + t(U))
  diag(U) <- runif(n)
  A <- (U < P) + 0 ;
  diag(A) <- 0

  avec <- A[col(A) > row(A)]
  pvec <- P[col(P) > row(P)]
  bvec <- numeric(n*(n-1)/2)

  uvec <- runif(n*(n-1)/2)

  idx1 <- which(avec == 1)
  idx0 <- which(avec == 0)

  bvec[idx1] <- (uvec[idx1] < (rho + (1 - rho)*pvec[idx1])) + 0
  bvec[idx0] <- (uvec[idx0] < (1 - rho)*pvec[idx0]) + 0

  B <- matrix(0, nrow = n, ncol = n)
  B[col(B) > row(B)] <- bvec
  B <- B + t(B)
  diag(B) <- 0

  return(list(A = igraph::graph.adjacency(A,"undirected"),
              B = igraph::graph.adjacency(B,"undirected")))
}

# Non-igraph version of correlated ER.
rg.sample.SBM.correlated <- function(n, B, rho, sigma, conditional = FALSE){
  if(!conditional){
    tau <- sample(c(1:length(rho)), n, replace = TRUE, prob = rho)
  }
  else{
    tau <- unlist(lapply(1:2,function(k) rep(k, rho[k]*n)))
  }
  P <- B[tau,tau]
  return(list(adjacency=rg.sample.correlated.gnp(P, sigma),tau=tau))
}

# HELPER METHODS

rg.sample.correlated.gnp <- function(P,sigma){
  n <-  nrow(P)
  U <- matrix(0, nrow = n, ncol = n)
  U[col(U) > row(U)] <- runif(n*(n-1)/2)
  U <- (U + t(U))
  diag(U) <- runif(n)
  A <- (U < P) + 0 ;
  diag(A) <- 0

  avec <- A[col(A) > row(A)]
  pvec <- P[col(P) > row(P)]
  bvec <- numeric(n*(n-1)/2)

  uvec <- runif(n*(n-1)/2)

  idx1 <- which(avec == 1)
  idx0 <- which(avec == 0)

  bvec[idx1] <- (uvec[idx1] < (sigma + (1 - sigma)*pvec[idx1])) + 0
  bvec[idx0] <- (uvec[idx0] < (1 - sigma)*pvec[idx0]) + 0

  B <- matrix(0, nrow = n, ncol = n)
  B[col(B) > row(B)] <- bvec
  B <- B + t(B)
  diag(B) <- 0

  return(list(A = igraph::graph.adjacency(A,"undirected"),
              B = igraph::graph.adjacency(B,"undirected")))
}
