#' Erdos-Renyi Graphs
#'
#' A function to simulate graphs from the Erdos-Renyi Random Graph Model.
#'
#' @param n the number of graphs to simulate.
#' @param v the number of vertices.
#' @param priors the composition of the graphs in each class as a length \code{K} vector. Defaults to \code{c(0.5, 0.5)}. Note that \code{K=length(priors)} is the number of classes of graphs, and \code{sum(priors)} will be normalized to \code{1}.
#' @param p the probability of an edge existing for each class, as a length \code{K} vector. Defaults to \code{c(0.5, 0.5)}. Note that \code{K=length(p)} is the number of classes of graphs, and \code{0 <= p <= 1}.
#'
#' @return An object of class \code{"Simulation"} containing the following:
#' \item{\code{graphs}}{\code{[[n]][v, v]} the \code{n} graphs with \code{v} vertices as a list of adjacency matrices.}
#' \item{\code{Y}}{\code{[n]} the class labels for each of the \code{n} graphs.}
#'
#' @author Eric Bridgeford
#' @export
gs.sims.er <- function(n, v, priors=c(0.5, 0.5), p=c(0.5, 0.5)) {

  # parameter checking
  K = gs.utils.check_params(list(priors=priors, p=p))

  # sample class labels
  labels <- sample(n, prob=priors)

  # sample individual graphs
  graphs <- lapply(1:n, function(i) {
    # sample edges w probability p for graph i
    matrix(rbinom(n=v^2, size=1, prob=p[labels[i]]), nrow=v)
  })
  return(structure(list(graphs=graphs, Y=labels), class="Simulation"))
}

#' Stochastic-Block Model Graphs
#'
#' A function to simulate graphs from the Stochastic Block Model (SBM).
#'
#' @param n the number of graphs.
#' @param v the number of vertices.
#' @param priors the composition of the graphs in each class as a length \code{K} vector. Defaults to \code{c(0.5, 0.5)}.
#' Note that \code{K=length(priors)} is the number of classes of graphs, and \code{sum(priors)} will be normalized to \code{1}.
#' @param C the community membership of each vertex in each class as a length \code{K} list of \code{length(r)} lists, where \code{r} is the number of communities for class \code{i}.
#' Note that \code{K=length(C)} is the number of classes of graphs, \code{union(C[[i]][[j]])} over all \code{j} communities for class \code{i} should be \code{1:v},
#' and \code{intersect(C[[i]][[j]])} for class \code{i} over all \code{j} communities should be empty.
#' Defaults to \code{list(list(seq(1, floor(n/2)), seq(floor(n/2)+1, n)), list(seq(1, floor(n/2)),seq(floor(n/2)+1, n)))}.
#' @param P the matrix of probabilities, where \code{P[[i]][k, l]} corresponds to the probability of an edge for class \code{i} between communities \code{C[[i]][[k]]} and \code{C[[i]][[l]]}.
#' Defaults to \code{list(matrix(rep(0.5, 4), nrow=2), matrix(rep(0.5, 4), nrow=2))}.
#'
#' @return An object of class \code{"Simulation"} containing the following:
#' \item{\code{graphs}}{\code{[[n]][v, v]} the \code{n} graphs with \code{v} vertices as a list of adjacency matrices.}
#' \item{\code{Y}}{\code{[n]} the class labels for each of the \code{n} graphs.}
#'
#' @examples
#' library(graphstats)
#' data <- gs.sims.sbm(100, 10)  # simulate 100 graphs with 10 vertices from the default model
#' @export
gs.sims.sbm <- function(n, v, priors=c(0.5, 0.5),
                        C=list(list(seq(1, floor(v/2)), seq(floor(v/2)+1, v)), list(seq(1, floor(v/2)), seq(floor(v/2)+1, v))),
                        P=list(matrix(rep(0.5, 4), nrow=2), matrix(rep(0.5, 4), nrow=2))) {

  # parameter checking
  K = gs.utils.check_params(list(priors=priors, C=C, P=P))
  # check C and P individaully for errors within since P and C should have same number of communities
  lapply(1:K, function(i) {
    Pmtx <- P[[i]]
    Clst <- C[[i]]
    r <- dim(Pmtx)[1]
    if (dim(Pmtx)[1] != dim(Pmtx)[2]) {
      stop(sprintf("You have not entered a valid Pmatrix. P[[%d]] has dimensions [%d, %d], but should be square.",
                   i, dim(Pmtx)[1], dim(Pmtx)[2]))
    }
    if (!all(1:v %in% unique(do.call(c, Clst)))) {
      stop(sprintf("You have not entered a valid vertex community. C[[%d]] does not contain a community for all vertices.", i))
    }
    if (length(do.call(intersect, Clst)) > 0) {
      stop(sprintf("You have not entered a valid vertex community C[[%d]] has one or more vertices in multiple communities.", i))
    }
    if (r != length(Clst)) {
      stop(sprintf("You have not entered a valid vertex community or Pmatrix. Class %d does not have the same number of communities specified in P or C.", i))
    }
  })

  # sample class labels
  labels <- sample(n, prob=priors)

  graphs <- lapply(1:n, function(i) {
    Pmtx <- P[[labels[i]]]
    Clst <- C[[labels[i]]]
    # empty matrix
    graph <- matrix(rep(0, v^2), nrow=v)
    for (k in 1:length(Clst)) {
      for (l in 1:length(Clst)) {
        # for community (k, l) sample with p=Pmtx[k, l]
        graph[Clst[[k]], Clst[[l]]] <- rbinom(length(Clst[[k]])*length(Clst[[l]]), size=1, prob=Pmtx[k, l])
      }
    }
    return(graph)
  })

  return(structure(list(graphs=graphs, Y=labels), class="Simulation"))
}

#' Structured Independent Edge Model Graphs
#'
#' A function to simulate graphs from the Structured Independent Edge Model (SIEM).
#'
#' @param n the number of graphs.
#' @param v the number of vertices.
#' @param priors the composition of the graphs in each class as a length \code{K} vector. Defaults to \code{c(0.5, 0.5)}.
#' Note that \code{K=length(priors)} is the number of classes of graphs, and \code{sum(priors)} will be normalized to \code{1}.
#' @param C the community membership of each edge in each class as a length \code{K} list of \code{length(r)} lists, where \code{r} is the number of communities for class \code{i}.
#' Note that \code{K=length(C)} is the number of classes of graphs, \code{union(C[[i]][[j]])} over all \code{j} communities for class \code{i} should be \code{1:v^2},
#' and \code{intersect(C[[i]][[j]])} for class \code{i} over all \code{j} communities should be empty.
#' Defaults to \code{list(list(seq(1, floor(n/2)), seq(floor(n/2)+1, n)), list(seq(1, floor(n/2)),seq(floor(n/2)+1, n)))}.
#' @param P the matrix of probabilities, where \code{P[[i]][k]} corresponds to the probability of an edge for class \code{i} in edge community \code{k}.
#' Defaults to \code{list(c(0.5, 0.5), c(0.5, 0.5))}.
#'
#' @return An object of class \code{"Simulation"} containing the following:
#' \item{\code{graphs}}{\code{[[n]][v, v]} the \code{n} graphs with \code{v} vertices as a list of adjacency matrices.}
#' \item{\code{Y}}{\code{[n]} the class labels for each of the \code{n} graphs.}
#'
#' @author Eric Bridgeford
#'
#' @examples
#' library(graphstats)
#' data <- gs.sims.siem(100, 10)  # simulate 100 graphs with 10 vertices from the default model
#' @export
gs.sims.siem <- function(n, v, priors=c(0.5, 0.5),
                        C=list(list(seq(1, floor(v^2/2)), seq(floor(v^2/2)+1, v^2)), list(seq(1, floor(v^2/2)),seq(floor(v^2/2)+1, v^2))),
                        P=list(c(0.5, 0.5), c(0.5, 0.5))) {

  # parameter checking
  K = gs.utils.check_params(list(priors=priors, C=C, P=P))
  # check C and P individaully for errors within since P and C should have same number of communities
  lapply(1:K, function(i) {
    Pmtx <- P[[i]]
    Clst <- C[[i]]
    r <- length(Pmtx)
    if (!all(1:v^2 %in% unique(do.call(c, Clst)))) {
      stop(sprintf("You have not entered a valid edge community. C[[%d]] does not contain a community for all edges.", i))
    }
    if (length(do.call(intersect, Clst)) > 0) {
      stop(sprintf("You have not entered a valid edge community C[[%d]] has one or more edges in multiple communities.", i))
    }
    if (r != length(Clst)) {
      stop(sprintf("You have not entered a valid edge community or Pmatrix. Class %d does not have the same number of communities specified in P or C.", i))
    }
  })

  # sample class labels
  labels <- sample(n, prob=priors)

  graphs <- lapply(1:n, function(i) {
    Pmtx <- P[[labels[i]]]
    Clst <- C[[labels[i]]]
    # empty matrix
    graph <- matrix(rep(0, v^2), nrow=v)
    for (k in 1:length(Clst)) {
      # for community (k) sample with p=Pmtx[k]
      graph[Clst] <- rbinom(length(Clst[[k]]), size=1, prob=Pmtx[k])
    }
    return(graph)
  })

  return(structure(list(graphs=graphs, Y=labels), class="Simulation"))
}

#' Sample Graph from Latent Position
#'
#' A function to simulate a binary graph given latent positions.
#'
#' @param X the latent positions, as a \code{n} by \code{k} matrix for \code{n} vertices and \code{k} blocks.
#' @param Y the second half of the latent positions for directed graphs, as a \code{n} by \code{k} matrix for \code{n} vertices and \code{k} blocks. Defaults to \code{NULL}, which assumes that the sampled graph is symmetric.
#' @return An \link[igraph]{graph} object with \code{n} vertices, with latent positions given by \code{X} if \code{G} is symmetric and \code{X}, \code{Y} if nonsymmetric.
#'
#' @author Eric Bridgeford
#'
#' @examples
#' library(graphstats)
#' data <- gs.sims.latent_pos(array(rnorm(100), dim=c(50, 2)))  # simulate 100 graphs with 10 vertices from the default model
#' @export
#'
gs.sims.latent_pos <- function(X, Y=NULL) {
  n <- dim(X)[1]; k <- dim(X)[2]
  G <- matrix(0, dim=c(n, n))
  if (is.null(Y)) {
    P <- X %*% t(X)
  } else {
      if (!all(dim(X) == dim(Y))) {
        stop("You have X and Y, but they do not have the same dimensionality.")
      }
    P <- X %*% t(Y)
  }
  for (i in 1:n) {
    for (j in 1:n) {
      G[i, j] <- rbinom(n=1, size=1, prob=P[i, j])
    }
  }
  if (is.null(Y)) {
    G[upper.tri(G, diag=FALSE)] <- 0
    G <- G + t(G) - diag(G)
  }
  return(G)
}

#' Stochastic-Block Model Graphs
#'
#' A function to check whether all parameters specified for a simulation are appropriately sized.
#'
#' @param params a named list of the model parameters. Assumed this list contains some element named \code{params$priors}.
#' @return K the number of classes to simulate graphs from.
#' @author Eric Bridgeford
gs.utils.check_params <- function(params) {
  K = length(params$priors)
  lapply(names(params), function(param) {
    paramobj <- params[[param]]
    if (length(paramobj) != K) {
      stop(sprintf("Parameter %s is of length %d, while length(priors)=%d.", param, K))
    }
  })
  return(K)
}
