#' Bernoulli Subgraph Computation
#'
#' \code{sg.bern.compute_subgraph} estimates the edges for a subgraph in a given set of samples.
#'
#' @param samp a list or array of graphs with arbitrary labelling.
#'     - if samp is a list, then it should have s elements of dimensions
#'         [n x n].
#'    - if samp is an array, then it should be of dimensions [n x n x s].
#' @param Y [s] the class labels.
#' @param e the number of edges for the subgraph.
#' @param coherent=FALSE a logical indicating whether to approximate a coherent, or incoherent, subgraph.
#' @param tstat="fisher" the test statistic to use. options are fisher's exact ("fisher") and chi-squared ("chisq").
#' @return subgraph [n x n] an array indicating whether an edge is present or not present in the subgraph.
#' @return p [n x n x c] the probability per edge of being connected per class for c classes.
#' @export
#' @seealso \code{\link{sg.bern.graph_estimator}}  \code{\link{sg.bern.subgraph_estimator}} \code{\link{sg.bern.subgraph_estimator}}
#'
sg.bern.compute_graph_statistics <- function(samp, Y, e, coherent=FALSE, tstat="fisher") {
  if(is.list(samp)) {
    samp <- fmriu.list2array(samp)  # convert to a array for standardization
  }
  dims <- dim(samp)


  if (tstat == "fisher") {
    tfunc <- fisher.test
  } else if (tstat == "chisq") {
    tfunc <- chisq.test
  } else {
    stop(sprintf(paste("You have entered an invalid test function. Options are:\n",
                       "\"fisher\", \"chisq\".", sep="")))
  }

  contingency <- sg.bern.contingency_matrix(samp, Y)
  cont_matrix <- contingency$cont_matrix
  p <- contingency$p
  pi <- contingency$pi

  test_results <- sg.bern.test_statistic(cont_matrix, tfunc)

  if (is_false(coherent)) {  # incoherent subgraph requested
    edge_idx <- sort(test_results, index.return=TRUE)$ix
  } else {
    stop("Coherent subgraph not implemented yet.")
  }

  return(list(subgraph=edge_idx, p=p, pi=pi))
}

#' A function to fit a bernoulli distribution to a given univariate samp of data.
#'
#' \code{sg.bern.estimator} estimate the parameter of a bernoulli distribution, p, for a given samp.
#'
#' @param samp a univariate samp of values with arbitrary class labels.
#' @param thresh=0: is the threshold below which we set edges to disconnected, and above which we set edges to connected.
#' @param smooth=TRUE: whether to smooth p to avoid undesirable limits.
#' @return p the p parameter per edge.
#' @export
#' @seealso \code{\link{sg.bern.graph_estimator}}
#'
sg.bern.estimator <- function(samp, thresh=0, smooth=TRUE) {
  samp <- ifelse(samp > thresh, 1, 0)  # force binarize
  s = length(samp)
  p=sum(samp)/s  # p is the number of 1s divided by number of samples total by defn
  if (smooth) {  # smooth if we only have 1 category so that we don't have p=0 or p=1, which has poor asymptotic behavior
    np <- 1/(10*s)
    p <- ifelse(p == 0, np, p)
    p <- ifelse(p == 1, 1 - np, p)
  }
  return(list(p=p))
}


#' A function to fit a bernoulli distribution to the edges of the matrices in a graph.
#'
#' \code{sg.bern.graph_estimator} uses the method of moments to estimate the parameters of a beta
#' distribution, alpha and beta, for a collection of graphs.
#'
#' @param samp a list or array of graphs with arbitrary labelling.
#'     - if samp is a list, then it should have s elements of dimensions
#'         [n x n].
#'    - if samp is an array, then it should be of dimensions [n x n x s].
#' @param thresh=0: is the threshold below which we set edges to disconnected, and above which we set edges to connected.
#' @param smooth=TRUE: whether to smooth p to avoid undesirable limits.
#' @return p [n x n] the p parameter per edge representing the probability of an edge existing.
#' @export
#' @seealso \code{\link{sg.bern.estimator}}
#'
sg.bern.graph_estimator <- function(samp, thresh=0, smooth=TRUE) {
  if(is.list(samp)) {
    samp <- fmriu.list2array(samp)  # convert to a array for standardization
  }

  samp <- 1*samp
  samp <- ifelse(samp > thresh, 1, 0)  # binarize to 1 if greater than thresh; 0 else

  dims <- dim(samp)
  n <- dims[1]
  m <- dims[2]
  s <- dims[3]

  if (n != m) {
    stop(sprintf("You have passed an invalid graph, as dim1 is %d and dim2 is %d, while dim1 should be == dim2.", n, m))
  }

  P <- apply(samp, c(1,2), sum)/s  # sum over third dimension and normalize by number of els for p per edge
  # smooth if desired
  if (smooth) {
    np <- 1/(10*s)
    P[P == 0] <- np
    P[P == 1] <- (1 - np)
  }
  return(list(p=P))
}


#' Sample Graphs from Bernoulli Distribution
#'
#' \code{sg.bern.sample_graph} A function to sample from a graph-valued RV where each edge follows a bernoulli distribution.
#'
#' @param p a [n x n] matrix indicating the alphas of each edge.
#' @param n the number of graphs to sample.
#' @param type an option of "list" or "array" (default) for the format of the output.
#' @param rewire the probability of arbitrarily rewiring edges.
#' @return sample the graph observations from the given beta distribution.
#'     - if type == "list", returns a p element list of [n x m] observations.
#'     - if type == "array" (default), returns a [n x m x s] element array where the 3rd dimension indexes the observations.
#' @export
#' @seealso \code{\link{list2array}} \code{\link{array2list}}
#'
sg.bern.sample_graph <- function(p, s=10, type="array", rewire=NaN, directed=FALSE) {
  dims <- dim(p)
  n <- dims[1]
  m <- dims[2]

  if (n != m) {
    stop(sprintf("You have passed an invalid graph, as dim1 is %d and dim2 is %d, while dim1 should be == dim2.", n, m))
  }

  samp <- array(runif(n=n*m*s), dim=c(n, m, s))

  # if x[,,i] < p, edge gets a 1, 0 otherwise
  # multiply by 1 to cast the logical array to numeric
  # apply over the third dimension (number of subjects) and reshape
  samp <- array(apply(samp, 3, function(x) {
    obs <- 1*(x < p)  # binary matrix
    if (!is.nan(rewire)) {  # if rewire arg is passed in, we want to randomly connect or disconnect edges with p=rewire
      rand_con <- 1*(array(runif(n*m), dim=c(n, m)) < rewire)  # uniform [0, 1)^{n x m} RV with probability p of being less than p
      rand_con[upper.tri(rand_con, diag=FALSE)] <- 0
      rand_con <-rand_con + t(rand_con) - diag(diag(rand_con))
      obs <- obs + rand_con
      obs[obs > 1] <- 0  # if any connected edges are incremented, should reset to disconnected
    }
    return(obs)
  }), dim=c(n, m, s))

  if (type == "array") {
    return(samp)
  } else if (type == "list") {
    return(array2list(samp))
  } else {
    stop(sprintf(paste("You have entered an invalid type %s.",
                       "Choices are \"list\" or \"array\"."), type))
  }
}

#' Bernoulli Subgraph Classifier
#'
#' \code{sg.bern.subgraph_classifier} classifies arbitrary data given the estimators of a subgraph.
#'
#' @param test the test array to generate labels for.
#'    - if test is an array, then it should be of dimensions [n x n x s].
#' @param p [n x n x c] the probability per edge of being connected per class for c classes.
#' @param pi [c] the pi vector probability of each class occuring.
#' @param classes the labels for the classes as ordered in the pi and p arrays.
#' @return edges [e] the edges present in the subgraph.
#' @return Yhat [s] the predicted class for each test example.
#' @export
#' @seealso \code{\link{sg.bern.compute_graph_statistics}}
#'
sg.bern.subgraph_classifier <- function(test, edges, p, pi, classes) {

  s <- dim(test)[3]
  c <- dim(p)[3]
  e <- length(edges)
  # convert to logarithmic in case ps are small
  pi <- log(pi)
  p <- log(p)
  hyp <- array(NaN, dim=c(s, e, c))
  for (k in 1:c) {
    pclass <- p[,,k]  # current probability matrix in log-form
    for (i in 1:s) {
      csamp <- test[,,i]  # current sample
      for (j in 1:e) {  # iterate over subgraph edges
        eix <- edges[j]  # id of the edge
        hyp[i, j, k] <- pclass[eix]*csamp[eix] + (1 - pclass[eix])*(1 - csamp[eix]) + pi[k]
      }
    }
  }

  h <- apply(hyp, c(1, 3), sum)  # product over the edges in the subgraph, the 3rd dimension

  Yhat <- apply(h, c(1), FUN = function(x) {
    classes[sort(x, decreasing=TRUE, index.return=TRUE)$ix[1]]
  })

  return(list(Yhat=Yhat))
}

#' Bernoulli Subgraph Edge Estimation
#'
#' \code{sg.bern.subgraph_classifier} orders the edges by test statistic results depending on the estimator type specified.
#'
#' @param tstats [n x n] the test-statistic results.
#' @param e the number of edges to look for, arbitrarily breaking ties as necessary.
#' @param coherent=FALSE if FALSE, estimate an incoherent subgraph, otherwise an integer indicating the number of vertices in the coherent subgraph.
#' @return edges [n*n] the edges in the graph ordered by test statistic depending on the estimator type.
#' @export
#' @seealso \code{\link{sg.bern.compute_graph_statistics}}
#'
sg.bern.subgraph_edge_estimation <- function(tstats, e, coherent=FALSE) {

  dims <- dim(tstats)
  n <- dims[1]
  m <- dims[2]

  if (n != m) {
    stop(sprintf("You have passed an invalid graph, as dim1 is %d and dim2 is %d, while dim1 should be == dim2.", n, m))
  }

  if (identical(coherent, FALSE)) {
    # incoherent estimator
    edges <- sort(tstats, index.return=TRUE)$ix[1:e]  # sort test statistics in ascending order and take first e of them
  } else if (is.numeric(coherent)) {
    if (coherent < 1) {
      stop(sprintf("The number of signal vertices you have requested, %d, is less than 1.", coherent))
    }
    coherent = ceiling(coherent)  # in case it is a float we ceil it
    # get the possible edge-wise unique values of significance we can have so we can increment over them
    # start with the lowest and add edges as we go
    unique_sig <- sort(unique(c(tstats)))
    for (sig in unique_sig) {
      vless <- 1*(tstats <= sig)  # 1s where lower than significance
      # score per vertex is the number of edges per vertex less than the threshold
      vertex_sig <- apply(vless, 1, sum) + apply(vless, 2, sum) - diag(vless)
      # see if we have (coherent #) vertices whose scores sum to greater than or equal the goal subgraph size
      vsig_ids_topc <- sort(vertex_sig, decreasing=TRUE, index.return=TRUE)$ix[1:coherent]
      if (sum(vertex_sig[vsig_ids_topc]) >= e) {
        # define a subgraph on the vertices with the most edges by setting the rest of the matrix to 1
        vertex_subgraph <- array(1, dim=c(n, n))
        # significance scores are btwn 0 and 1 and smaller is what we want, so fill in our subgraph with
        # the true values which will be lower than 1 assuming any entries have a chance to reject the null
        vertex_subgraph[vsig_ids_topc,] <- tstats[vsig_ids_topc,]
        vertex_subgraph[,vsig_ids_topc] <- tstats[,vsig_ids_topc]
        # find the top edges from the vertex subgraph as the ones with lowest e significances
        edges <- sort(vertex_subgraph, index.return=TRUE)$ix[1:e]
        break
      }
    }
  } else {
    stop("You have entered an invalid number of signal vertices.")
  }
  return(edges)
}

#' Bernoulli Subgraph Estimator
#'
#' \code{sg.beta.subgraph_estimator} A function to produce estimators for bernoulli graphs needed to compute subgraphs.
#'
#' @param samp  [n x n x s] an array of graph samples. Should be binary (0 or 1).
#' @param Y [s] the class labels.
#' @return cont_matrix [n, n, c, 2] a contingency table for each edge, where each contingency
#'         table has either connected or unconnected for each class.
#' @export
#' @seealso \code{\link{sg.bern.graph_estimator}}
#'
sg.bern.subgraph_estimator <- function(samp, Y) {
  dims <- dim(samp)
  n <- dims[1]
  m <- dims[2]
  s <- dims[3]

  if (n != m) {
    stop(sprintf("You have passed an invalid graph, as dim1 is %d and dim2 is %d, while dim1 should be == dim2.", n, m))
  }

  if (s != length(Y)) {
    stop(sprintf(paste('The number of samples, ', s, "is not equal to\n",
                       " the number of class labels, ", length(Y), '.', sep="")))
  }

  classes <- unique(Y)  # classes are our unique class labels
  C <- length(classes)

  cont_matrix <- array(NaN, dim=c(n, n, C, 2))
  p = array(NaN, dim=c(n, n, C))
  pi = array(NaN, dim=c(C))

  # make the contingency matrix
  for (k in 1:C) {
    class <- classes[k]
    idx_class <- (Y == class)
    # class sample is where the class label is the current class
    class_samp <- samp[,, idx_class]
    # add current clas to the contingency table
    cont_matrix[,, k, 1] <- apply(class_samp, c(1, 2), sum)  # connected edges are the number of 1-valued edges
    cont_matrix[,, k, 2] <- sum(idx_class) - cont_matrix[,, k, 1]  # disconnected edges

    # estimate p for the current class
    p[,, k] <- sg.bern.graph_estimator(class_samp, smooth=TRUE)$p
    pi[k] <- sum(1*idx_class)/s
  }

  return(list(cont_matrix=cont_matrix, p=p, pi=pi, classes=classes))
}

#' Bernoulli Subgraph Train
#'
#' \code{sg.bern.xval_classifier} Trains a Model for identification of the edges in a subgraph.
#'
#' @param samp a list or array of graphs with arbitrary labelling.
#'     - if samp is a list, then it should have s elements of dimensions
#'         [n x n].
#'    - if samp is an array, then it should be of dimensions [n x n x s].
#' @param Y [s] the class labels.
#' @param e [1] the number of edges in the subgraph.
#' @param coherent=FALSE if FALSE, estimate an incoherent subgraph, otherwise an integer indicating the number of vertices in the coherent subgraph.
#' @param tstat="fisher" the test statistic to use. options are fisher's exact ("fisher") and chi-squared ("chisq").
#' @return edges [n x n] an ordering of the edges in the subgraph by test statistic results.
#' @return p [n x n x c] the probability per edge of being connected per class for c classes.
#' @return pi [c] the probability of seeing a given class.
#'
#' @export
#' @seealso \code{\link{sg.bern.compute_graph_statistics}}
#'
sg.bern.subgraph_train <- function(samp, Y, e, coherent=FALSE, tstat="fisher") {
  # estimate basic parameters for the subgraph
  sg_ests <- sg.bern.subgraph_estimator(samp, Y)
  # compute test statistics per edge from the contingency matrix
  tstats <- sg.bern.edge_test(sg_ests$cont_matrix, test_stat=tstat)
  # estimate the ordering of edges depending on the test statistic results
  edges <- sg.bern.subgraph_edge_estimation(tstats, e, coherent=coherent)
  return(list(edges=edges, p=sg_ests$p, pi=sg_ests$pi, classes=sg_ests$classes))
}

#' Cross-Validated Bernoulli Subgraph Classifier
#'
#' \code{sg.bern.xval_classifier} Bernoulli Subgraph Classifier with Cross Validation to determine the optimal subgraph.
#'
#' @param samp a list or array of graphs with arbitrary labelling.
#'     - if samp is a list, then it should have s elements of dimensions
#'         [n x n].
#'    - if samp is an array, then it should be of dimensions [n x n x s].
#' @param Y [s] the class labels.
#' @param nedges [z] an array where each element is the number of edges to look for, arbitrarily breaking ties as necessary.
#' @param coherent=FALSE if FALSE, estimate an incoherent subgraph, otherwise an integer indicating the number of vertices in the coherent subgraph.
#' @param tstat="fisher" the test statistic to use. options are fisher's exact ("fisher") and chi-squared ("chisq").
#' @param xval="loo" the cross-validation options to use. Options are "loo" (leave-one-out) and "kfold" (K-fold).
#' @param folds=NaN the number of folds to do if xval is set to kfold.
#' @return subgraph [n x n] an array indicating whether an edge is present or not present in the subgraph.
#' @return p [n x n x c] the probability per edge of being connected per class for c classes.
#' @export
#' @seealso \code{\link{sg.bern.compute_graph_statistics}}
#'
sg.bern.xval_classifier <- function(samp, Y, nedges, coherent=FALSE, tstat="fisher", xval="loo", folds=NaN) {

  if(is.list(samp)) {
    samp <- fmriu.list2array(samp)  # convert to a array for standardization
  }

  dims <- dim(samp)
  n <- dims[1]
  m <- dims[2]
  s <- dims[3]

  if (n != m) {
    stop(sprintf("You have passed an invalid graph, as dim1 is %d and dim2 is %d, while dim1 should be == dim2.", n, m))
  }

  if (s != length(Y)) {
    stop(sprintf(paste('The number of samples, ', s, "is not equal to\n",
                       " the number of class labels, ", length(Y), '.', sep="")))
  }

  subgraphs <- list()

  if (xval == "loo") {
    # iterate over the element that is going to be held out
    er <- 0
    for (i in 1:s) {
      # split into training and validation sets
      splits <- gs.xval.split(samp, Y, i)
      train_set <- splits$train_set
      train_y <- splits$train_y
      test_set <- splits$test_set
      test_y <- splits$test_y
      # estimators for graph
      sg_ests <- sg.bern.subgraph_train(samp = train_set, Y = train_y, nedges, coherent=coherent, tstat=tstat)

      # classify the testing data and produce accuracy summary
      test_pred <- sg.bern.subgraph_classifier(test_set, sg_ests$edges, sg_ests$p, sg_ests$pi, sg_ests$classes)
      if (test_pred$Yhat[1] != test_y[1]) {
        er <- er + 1  # classification mistake, so increment error counter
      }
    }
    er <- er/s  # error is number of misclassifications / number of possible samples
    # train the model on the full data for the actual result
    sg_ests <- sg.bern.subgraph_train(samp, Y, nedges, coherent=coherent, tstat=tstat)
    result <- list(method="loo", nedges=nedges, coherent=coherent, n=s,
                   test=tstat, folds=NaN, error=er, edges=sg_ests$edges)
  } else {
    stop('You have passed an unsupported cross-validatioon method.')
  }
  return(result)
}


#' Bernoulli Subgraph Edge Test
#'
#' \code{sg.bern.edge_test} A function to compute the test statistic per edge from a contingency matrix.
#'
#' @param cont_matrix [n x n x c x 2] a contingency table for each edge, where each contingency
#'         table has either connected or unconnected for each class.
#' @param test the test statistic function object to use. Options are "fisher" or "chisq".
#' @return test_stat [n x n] the test statistic per edge.
#' @export
#' @seealso \code{\link{sg.beta.subgraph_estimator}}
#'
gs.ssg.bern.edge_test <- function(cont_matrix, test_stat="fisher") {

  if (test_stat == "fisher") {
    tfunc <- fisher.test
  } else if (test_stat == "chisq") {
    tfunc <- chisq.test
  } else {
    print(paste("You have entered an invalid test function. Options are:\n",
                "\"fisher\", \"chisq\".", sep=""))
  }
  dims <- dim(cont_matrix)
  n <- dims[1]
  m <- dims[2]

  if (n != m) {
    stop(sprintf("You have passed an invalid graph, as dim1 is %d and dim2 is %d, while dim1 should be == dim2.", n, m))
  }

  test_stats <- array(NaN, dim=c(n, n))
  for (i in 1:n) {
    for (j in 1:n) {
      # compute given test statistic on the contingency matrix for that edge
      test_stats[i, j] <- do.call(tfunc, list(cont_matrix[i, j,,]))$p
    }
  }

  return(test_stats)
}
