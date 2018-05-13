#' Generates a random matrix to use as a
#'  starting point of the multistart (SGM-step) algorithm. For some
#'  \eqn{alpha} in (0,\code{g}), output matrix is weighted towards the bary-center
#'  by \eqn{(1-alpha)} and towards another randomly generated
#'  doubly-stochastic matrix by \eqn{alpha}.
#'
#' @param n the dimension to make the output matrix
#' @param g (short for gamma) a number between 0 and 1 input
#'                 by the user as an upper bound for step \eqn{alpha}
#' @return a random doubly stochastic matrix
#'
#' @author Heather Gaddy Patsolic <hgaddy1@jhu.edu>
#' @export
# S.D.G.
rsp<-function(n,g){

  s <- sample(n);s
  I <- diag(n);I
  P <- I[s,];P
  alpha <- runif(1,0,g);alpha
  J <- matrix(1,n,n); # J
  bc <- (1/n)*J #this is the barycenter
  #    bc
  P <- (1-alpha)*bc + alpha*P

  return(P)

}
