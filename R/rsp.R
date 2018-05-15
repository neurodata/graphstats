#' Find a suitable initial position for multistart SGM
#'
#' Generates a random matrix to use as a starting point of the
#' multistart SGM algorithm. For some \eqn{alpha} in (0,\code{gamma}),
#' output matrix is weighted towards the barycenter by \eqn{(1-alpha)}
#' and towards another randomly generated doubly-stochastic matrix by
#' \eqn{alpha}.
#'
#' @param n the dimension to make the output matrix
#' @param gamma a number between 0 and 1 input
#' by the user as an upper bound for step \eqn{alpha}
#' @return a random doubly stochastic matrix
#'
#' @author Heather Gaddy Patsolic <hgaddy1@jhu.edu>
#' @export

rsp <- function(n, gamma) {

  s <- sample(n)
  I <- diag(n)
  P <- I[s,]
  alpha <- runif(1,0,gamma)
  J <- matrix(1,n,n)
  bc <- (1/n)*J # this is the barycenter
  P <- (1-alpha)*bc + alpha*P

  return(P)

}
