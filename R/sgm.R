#' Matches Graphs given a seeding of vertex correspondences.
#'
#' Given two adjacency matrices \code{A} and \code{B} of the same size, match
#' the two graphs with the help of \code{m} seed vertex pairs which correspond
#' to \code{m} rows (and columns) of the adjacency matrices.
#'
#' The approximate graph matching problem is to find a bijection between the
#' vertices of two graphs, such that the number of edge disagreements between
#' the corresponding vertex pairs is minimized. For seeded graph matching, part
#' of the bijection that consists of known correspondences (the seeds) is known
#' and the problem task is to complete the bijection by estimating the
#' permutation matrix that permutes the rows and columns of the adjacency
#' matrix of the second graph.
#'
#' It is assumed that for the two supplied adjacency matrices \code{A} and
#' \code{B}, both of size \eqn{n\times n}{n*n}, the first \eqn{m} rows(and
#' columns) of \code{A} and \code{B} correspond to the same vertices in both
#' graphs. That is, the \eqn{n \times n}{n*n} permutation matrix that defines
#' the bijection is \eqn{I_{m} \bigoplus P} for a \eqn{(n-m)\times
#' (n-m)}{(n-m)*(n-m)} permutation matrix \eqn{P} and \eqn{m} times \eqn{m}
#' identity matrix \eqn{I_{m}}. The function \code{match_vertices} estimates
#' the permutation matrix \eqn{P} via an optimization algorithm based on the
#' Frank-Wolfe algorithm.
#'
#' See references for further details.
#'
# @aliases match_vertices seeded.graph.match
#' @param A a numeric matrix, the \eqn{n \times n}{n*n} adjacency matrix of the first graph
#' @param B a numeric matrix, the \eqn{n \times n}{n*n} adjacency matrix of the second graph
#' @param seeds a numeric matrix, the \eqn{m \times 2}{m*2} matching vertex table.
#' If \code{S} is \code{NULL}, then it is using a \eqn{soft} seeding algorithm.
#' @param hard a boolean, TRUE for hard seeding, FALSE for soft seeding.
#' @param pad a scalar value for padding.
#' @param start ?
#' @param maxiter The number of maxiters for the Frank-Wolfe algorithm.
#' @return A numeric matrix which is the permutation matrix that determines the
#' bijection between the graphs of \code{A} and \code{B}
#' @author Vince Lyzinski \url{http://www.ams.jhu.edu/~lyzinski/}
#' @references Vogelstein, J. T., Conroy, J. M., Podrazik, L. J., Kratzer, S.
#' G., Harley, E. T., Fishkind, D. E.,Vogelstein, R. J., Priebe, C. E. (2011).
#' Fast Approximate Quadratic Programming for Large (Brain) Graph Matching.
#' Online: \url{http://arxiv.org/abs/1112.5507}
#'
#' Fishkind, D. E., Adali, S., Priebe, C. E. (2012). Seeded Graph Matching
#' Online: \url{http://arxiv.org/abs/1209.0367}
#'
#' @export
sgm <- function(A,B,seeds,hard=TRUE,pad=0,start="barycenter",maxiter=20){

  validateInput(A,B,seeds,hard,pad,maxiter)

  gamma <- 0.1
  nv1<-nrow(A)
  nv2<-nrow(B)
  nv<-max(nv1,nv2)

  if(is.null(seeds)) { # no seed!
    m=0
    if (start=="barycenter") {
      S<-matrix(1/nv,nv,nv)
    } else {
      S <- rsp(nv,gamma)
    }
    AA <- A
    BB <- B
  }else {
    A.ind <- c(seeds[,1], setdiff(1:nv1, seeds[,1]))
    B.ind <- c(seeds[,2], setdiff(1:nv2, seeds[,2]))
    AA <- A[A.ind, A.ind]
    BB <- B[B.ind, B.ind]

    m <- nrow(seeds)
    if (hard==TRUE) {
      n <- nv-m
      if (start=="barycenter") {
        S <- matrix(1/n,n,n)
      } else {
        S <- rsp(n,gamma)
      }
    } else {
      s <- m
      m <- 0
      if (start=="barycenter") {
        diag1 <- diag(s)
        diag2 <- matrix(1/(nv-s),nv-s,nv-s)
        offdiag <- matrix(0,s,nv-s)
        S <- rbind(cbind(diag1,offdiag), cbind(t(offdiag),diag2))
      } else {
        M <- rsp(nv-s,gamma)
        S <- diag(nv);
        S[(s+1):nv,(s+1):nv] <- M
      }
    }
  }

  P <- sgm.ordered(AA,BB,m,S,pad,maxiter)
  return(P)
}


#' Matches Graphs given a seeding of vertex correspondences
#'
#' Given two adjacency matrices \code{A} and \code{B} of the same size, match
#' the two graphs with the help of \code{m} seed vertex pairs which correspond
#' to the first \code{m} rows (and columns) of the adjacency matrices.
#'
#' The approximate graph matching problem is to find a bijection between the
#' vertices of two graphs , such that the number of edge disagreements between
#' the corresponding vertex pairs is minimized. For seeded graph matching, part
#' of the bijection that consist of known correspondences (the seeds) is known
#' and the problem task is to complete the bijection by estimating the
#' permutation matrix that permutes the rows and columns of the adjacency
#' matrix of the second graph.
#'
#' It is assumed that for the two supplied adjacency matrices \code{A} and
#' \code{B}, both of size \eqn{n\times n}{n*n}, the first \eqn{m} rows(and
#' columns) of \code{A} and \code{B} correspond to the same vertices in both
#' graphs. That is, the \eqn{n \times n}{n*n} permutation matrix that defines
#' the bijection is \eqn{I_{m} \bigoplus P} for a \eqn{(n-m)\times
#' (n-m)}{(n-m)*(n-m)} permutation matrix \eqn{P} and \eqn{m} times \eqn{m}
#' identity matrix \eqn{I_{m}}. The function \code{match_vertices} estimates
#' the permutation matrix \eqn{P} via an optimization algorithm based on the
#' Frank-Wolfe algorithm.#' Matches Graphs given a seeding of vertex correspondences.
#'
#' Given two adjacency matrices \code{A} and \code{B} of the same size, match
#' the two graphs with the help of \code{m} seed vertex pairs which correspond
#' to \code{m} rows (and columns) of the adjacency matrices.
#'
#' The approximate graph matching problem is to find a bijection between the
#' vertices of two graphs, such that the number of edge disagreements between
#' the corresponding vertex pairs is minimized. For seeded graph matching, part
#' of the bijection that consists of known correspondences (the seeds) is known
#' and the problem task is to complete the bijection by estimating the
#' permutation matrix that permutes the rows and columns of the adjacency
#' matrix of the second graph.
#'
#' It is assumed that for the two supplied adjacency matrices \code{A} and
#' \code{B}, both of size \eqn{n\times n}{n*n}, the first \eqn{m} rows(and
#' columns) of \code{A} and \code{B} correspond to the same vertices in both
#' graphs. That is, the \eqn{n \times n}{n*n} permutation matrix that defines
#' the bijection is \eqn{I_{m} \bigoplus P} for a \eqn{(n-m)\times
#' (n-m)}{(n-m)*(n-m)} permutation matrix \eqn{P} and \eqn{m} times \eqn{m}
#' identity matrix \eqn{I_{m}}. The function \code{match_vertices} estimates
#' the permutation matrix \eqn{P} via an optimization algorithm based on the
#' Frank-Wolfe algorithm.
#'
#' See references for further details.
#'
# @aliases match_vertices seeded.graph.match
#' @param A a numeric matrix, the adjacency matrix of the first graph
#' @param B a numeric matrix, the adjacency matrix of the second graph
#' @param m The number of seeds. The first \code{m} vertices of both graphs are
#' matched.
#' @param start a numeric matrix, the permutation matrix estimate is
#' initialized with \code{start}.
#' @param pad a scalar value for padding
#' @param maxiter The number of maxiters for the Frank-Wolfe algorithm
#' @param LAP a character either "exact" or "approx"
#' @return A numeric matrix which is the permutation matrix that determines the
#' bijection between the graphs of \code{A} and \code{B}
#' @author Vince Lyzinski \url{http://www.ams.jhu.edu/~lyzinski/}
#' @references Vogelstein, J. T., Conroy, J. M., Podrazik, L. J., Kratzer, S.
#' G., Harley, E. T., Fishkind, D. E.,Vogelstein, R. J., Priebe, C. E. (2011).
#' Fast Approximate Quadratic Programming for Large (Brain) Graph Matching.
#' Online: \url{http://arxiv.org/abs/1112.5507}
#'
#' Fishkind, D. E., Adali, S., Priebe, C. E. (2012). Seeded Graph Matching
#' Online: \url{http://arxiv.org/abs/1209.0367}
#'
#' @export
sgm.ordered <- function(A,B,m,start,pad=0,maxiter=20,LAP="exact",verbose=FALSE){

  # In this function, seeds are assumed to be vertices 1:m in both graphs.
  validateInput.ordered(A,B,m,start,pad,maxiter,LAP,verbose)

  # Collect the number of vetices in each graph.
  totv1<-ncol(A)
  totv2<-ncol(B)

  # In the case that the number of vertices differ,
  # we must pad the smaller graph with the value given by parameter pad.
  if (totv1>totv2) {
    A[A==0]<- -1
    B[B==0]<- -1
    diff<-totv1-totv2
    B <- cbind(B, matrix(pad, nrow(B), diff))
    B <- rbind(B, matrix(pad, diff, ncol(B)))
  } else if (totv1<totv2) {
    A[A==0]<- -1
    B[B==0]<- -1
    diff<-totv2-totv1
    A <- cbind(A, matrix(pad, nrow(A), diff))
    A <- rbind(A, matrix(pad, diff, ncol(A)))
  }
  # Here, n represents the number of non-seeded vertices.
  totv <- max(totv1,totv2)
  n <- totv-m

  # Partition the given matrices into their seeded and non-seeded quadrants.
  # This is described on page 5 of Fishkind et al. (2018)
  # <https://arxiv.org/pdf/1209.0367.pdf>.
  if (m==0){
    A12 <- A21 <- B12 <- B21 <- matrix(0,n,n)
  } else {
    A12<-rbind(A[1:m,(m+1):(m+n)])
    A21<-cbind(A[(m+1):(m+n),1:m])
    B12<-rbind(B[1:m,(m+1):(m+n)])
    B21<-cbind(B[(m+1):(m+n),1:m])
  }
  if (n==1) {
    A12 <- A21 <- B12 <- B21 <- t(A12)
  }
  A22<-A[(m+1):(m+n),(m+1):(m+n)]
  tA22 <- t(A22)
  B22<-B[(m+1):(m+n),(m+1):(m+n)]
  tB22 <- t(B22)

  # Solve the QAP using the Frank-Wolfe algorithm.
  tol<-1
  P<-start
  toggle<-1
  iter<-0
  x<- A21 %*% t(B21)
  y<- t(A12) %*% B12
  xy <- x + y
  while (toggle==1 & iter<maxiter) {

    iter<-iter+1
    z <- A22 %*% P %*% tB22
    w <- tA22 %*% P %*% B22
    Grad <- xy+z+w;

    if (LAP=="exact") {
      # Solve exactly.
      mm <- max(abs(Grad))
      ind<-matrix(clue::solve_LSAP(Grad+matrix(mm,totv-m,totv-m), maximum =TRUE))
    } else {
      # Solve approximately.
      temp <- matrix(0, n, n)
      Grad1 <- rbind(cbind(temp, t(Grad)),cbind(Grad, temp))
      ind <- parallelMatch(Grad1)
    }

    Pdir <- diag(n)
    Pdir <- Pdir[ind,]
    tPdir <- t(Pdir)
    tP <- t(P)
    wt <- tA22 %*% Pdir %*% B22
    c <- sum(diag(w %*% tP))
    d <- sum(diag(wt %*% tP)) + sum(diag(w %*% tPdir))
    e <- sum(diag(wt %*% tPdir))
    u <- sum(diag(tP %*% x + tP %*% y))
    v <- sum(diag(tPdir %*% x + tPdir %*% y))
    if( c-d+e==0 && d-2*e+u-v==0){
      alpha <- 0
    }else{
      alpha <- -(d-2*e+u-v)/(2*(c-d+e))}
    f0 <- 0
    f1 <- c-e+u-v
    falpha <- (c-d+e)*alpha^2+(d-2*e+u-v)*alpha
    if (alpha < tol && alpha > 0 && falpha > f0 && falpha > f1) {
      P <- alpha*P+(1-alpha)*Pdir
    } else if (f0 > f1) {
      P <- Pdir
    } else {
      toggle<-0}
    if (verbose) cat("iter = ", iter, "\n")
  }
  D<-P

  if (LAP=="exact") {
    corr<-matrix(clue::solve_LSAP(P, maximum = TRUE))
  } else {
    PP <- rbind(cbind(temp, t(P)),cbind(P, temp))
    corr<- t(parallelMatch(PP))
  }
  P=diag(n)
  P=rbind(cbind(diag(m),matrix(0,m,n)),cbind(matrix(0,n,m),P[corr,]))
  corr<-cbind(matrix((m+1):totv, n),matrix(m+corr,n))

  #return(list(corr=corr[,2], P=P, D=D, iter=iter))
  return(P)
}

# HELPER METHODS

# Function to solve Linear Assignment Problem
# Using algorithm in A multithreaded algorithm for network alignment
# via approximate matching
# graph has the form: [0 C; C' 0] where C is the cost function
# Output mate is a permutation
# Written by Ao Sun and Lingyao Meng
#' @export
parallelMatch <- function(graph) {

  ## adjacency matrix corresponding to graph G
  numVertices <- nrow(graph)

  ## Initialize the matching vector with NaN
  mate <- rep(NaN, numVertices)

  ## Initialize the other parameters
  candidate <- rep(0, numVertices);
  qC <- vector(); qN <- vector();

  #--------------------------------- Phase 1 ------------------------------------------
  candidate <- sapply(seq_len(numVertices), function(x) findMate(x,graph,mate))

  for (j in 1 : numVertices) {
    temp <- matchVertex(j, candidate, mate, qC);
    qC <- temp[[1]]; mate <- temp[[2]];
  }
  #--------------------------------- Phase 2 ------------------------------------------
  repeat {
    for (k in 1 : length(qC)) {
      ## Return to the index of adjacent Vertex
      if (qC[k] <= (numVertices / 2)) {

        for (h in (numVertices / 2 + 1) : numVertices) {
          if ((candidate[h] == qC[k]) && (h != mate[qC[k]])) {
            candidate[h] <- findMate(h, graph, mate);
            temp2 <- matchVertex(h, candidate, mate, qN);
            qN <- unlist(temp2[1]); mate <- unlist(temp2[2]);
          }
        }
      } else {
        for (h in 1 : (numVertices / 2)) {
          if ((candidate[h] == qC[k]) && h != mate[qC[k]]) {
            candidate[h] <- findMate(h, graph, mate);
            temp3 <- matchVertex(h, candidate, mate, qN);
            qN <- unlist(temp3[1]); mate <- unlist(temp3[2]);
          }
        }
      }
    }

    qC <- qN;
    qN <- vector();
    if (length(qC) == 0) {
      break;
    }
  }
  mate <- mate[(numVertices / 2 + 1) : length(mate)];
  return(mate)
}

## for aLAP
# Help method of parrallelMatch
matchVertex <- function(s, candidate, mate, Q) {
  if (candidate[candidate[s]] == s) {
    mate[s] <- candidate[s];
    mate[candidate[s]] <- s;
    Q <- c(Q, s, candidate[s]);
  }
  return(list(Q, mate));
}

findMate <- function(s, graph, mate) {
  # Initialization
  max_wt <- -Inf;
  max_wt_id <- NaN;

  # Find the locally dominant vertices for one single vertex
  if (s <= dim(graph)[2] / 2) {
    for (i in (dim(graph)[2] / 2 + 1) : dim(graph)[2]) {
      if (is.nan(mate[i]) && max_wt < graph[s, i]) {
        max_wt <- graph[s, i];
        max_wt_id <- i;
      }
    }
  } else {
    for (i in 1 : (dim(graph)[2] / 2)) {
      if (is.nan(mate[i]) && max_wt < graph[s, i]) {
        max_wt <- graph[s, i];
        max_wt_id <- i;
      }
    }
  }
  return(max_wt_id);
}

# Unordered function validation.
validateInput <- function(A,B,seeds,hard,pad,maxiter) {

  # Matrices
  if (class(A) != "matrix" && class(A) != "dgCMatrix") { stop("Error: Input 'A' must be a matrix.") }
  if (class(B) != "matrix" && class(B) != "dgCMatrix") { stop("Error: Input 'B' must be a matrix.") }
  if (nrow(A) != ncol(A)) { stop("Error: 'A' is not a square matrix.")}
  if (nrow(B) != ncol(B)) { stop("Error: 'B' is not a square matrix.")}

  # Seeds
  if (class(seeds) != "matrix" || ncol(seeds) != 2 || !all(seeds%%1 == 0)) {
    stop("Error: Input 'seeds' must be an m by 2 matrix of corresponding seed indices.")
  }
  if (nrow(seeds) > nrow(A)) { stop("Error: Number of seeds is greater than number of vertices in A.") }
  if (nrow(seeds) > nrow(B)) { stop("Error: Number of seeds is greater than number of vertices in B.") }

  # Hard/Soft
  if (!is.logical(hard)) { stop("Error: Input 'hard' must be a logical.")}

  # Padding
  if (!is.numeric(pad)) { stop("Error: Input 'pad' must be a number.")}

  # Iterations
  if (!is.numeric(maxiter) || maxiter%%1 != 0) { stop("Error: Input 'maxiter' must be an integer.") }
  if (maxiter < 0) { stop("Error: Iterations 'maxiter' is less than 0.") }

}

# Ordered function validation.
validateInput.ordered <- function(A,B,m,start,pad,maxiter,LAP,verbose) {

  # Matrices
  if (class(A) != "matrix" && class(A) != "dgCMatrix") { stop("Error: Input 'A' must be a matrix.") }
  if (class(B) != "matrix" && class(B) != "dgCMatrix") { stop("Error: Input 'B' must be a matrix.") }
  if (nrow(A) != ncol(A)) { stop("Error: 'A' is not a square matrix.") }
  if (nrow(B) != ncol(B)) { stop("Error: 'B' is not a square matrix.") }

  # Number of Seeds
  if (!is.numeric(m) || m%%1 != 0 || m < 0) { stop("Error: Input 'm', number of seeds, must be an integer and >=0.")}

  # Start
  totv <- max(ncol(A),ncol(B))
  n <- totv - m
  if (class(start) != "matrix" && class(start) != "dgCMatrix") { stop("Error: Input 'start' must be a matrix.") }
  if (nrow(start) != ncol(start) || nrow(start) != n) { stop("Error: Input 'start' must be a square, n-m by n-m matrix.")}

  # Padding
  if (!is.numeric(pad)) { stop("Error: Input 'pad' must be a number.")}

  # Iterations
  if (!is.numeric(maxiter) || maxiter%%1 != 0) { stop("Error: Input 'maxiter' must be an integer.") }
  if (maxiter < 0) { stop("Error: Iterations 'maxiter' is less than 0.") }

  # LAP
  if (LAP != "exact" && LAP != "approx") { stop("Error: LAP must be a string equal to 'exact' or 'approx'.") }

  # Verbosity
  if (!is.logical(verbose)) { stop("Error: Input 'verbose' must be a logical.")}

}
