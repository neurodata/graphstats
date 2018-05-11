# Function to solve Linear Assignment Problem
# Using algorithm in A multithreaded algorithm for network alignment
# via approximate matching
# graph has the form: [0 C; C' 0] where C is the cost function
# Output mate is a permutation
# Written by Ao Sun and Lingyao Meng
parallelMatch <- function(graph) {
  ##require('foreach')
  ##source('findMate.R');
  ##source('matchVertex.R');
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
