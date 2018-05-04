context("Laplacian Spectral Embedding")

test_that("Too low, too high, or non-integer 'dim' value.", {

  # Define simple 2-vertex graph.
  n <- 2
  A <- matrix(c(0, 1, 1, 0), nrow = n)
  g <- igraph::graph_from_adjacency_matrix(A)

  # Less than 1, or greater than the number of vertices.
  dim <- 0
  expect_error(lse(g, dim), "Number of dimensions 'dim' is less than 1.")
  dim <- -10
  expect_error(lse(g, dim), "Number of dimensions 'dim' is less than 1.")
  dim <- n + 1
  expect_error(lse(g, dim), "Num. Embedded dimensions 'dim' is greater than number of vertices.")

  # Not a single number.
  dim <- "string"
  expect_error(lse(g, dim), "Input 'dim' is not a number.")
  dim <- g
  expect_error(lse(g, dim), "Input 'dim' has length > 1.")
  dim <- matrix(c(1, 0, 0, 1), nrow = 2)
  expect_error(lse(g, dim), "Input 'dim' has length > 1.")
  dim <- c(5, 6, 7)
  expect_error(lse(g, dim), "Input 'dim' has length > 1.")

})

test_that("Incorrect input graph 'g'.", {

  # Not a graph or a matrix of any kind.
  g <- "string"
  dim <- 1
  expect_error(lse(g, dim), "Input object 'g' is not an igraph object.")

  # 'g' contains multiple graphs.
  A1 <- matrix(c(1, 0, 0, 1), nrow = 2)
  A2 <- matrix(c(1, 0, 0, 1), nrow = 2)
  A <- c(A1, A2)
  dim <- 1
  expect_error(lse(A, dim), "Input object 'g' is not an igraph object.")

  # Matrix, but not a valid adjacency (square) matrix.
  A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  dim <- 1
  expect_error(lse(A, dim), "Non-square matrix, Non-square matrix")
})

# Simulation function for SBM. Returns adjacency matrix.
simulate_sbm2 <- function(n, B, assignments) {

  A <- matrix(rep(0, n*n), nrow = n)
  for (i in 2:n) {
    for (j in 1:(i-1)) {
      A[i, j] <- rbinom(1, 1, B[assignments[i], assignments[j]] )
      A[j, i] <- A[i, j]
    }
  }
  A
}

# General Functionality.
test_that("End-to-end testing.", {

  # Number of simulations. Count how many times latent block assignments are recovered
  # via kmeans clustering of ASE from a core-periphery 2-SBM and simple random graph.
  num_sims <- 10
  cp_is_better <- 0
  er_is_better <- 0

  for (s in 1:num_sims) {

    ## Simulate  core-periphery SBM, and simple ER graph.
    set.seed(123)
    n <- 100
    assignments <- c(rep(1, n/2), rep(2, n/2))

    # Block to block edge probabilities.
    B_cp <- matrix(c(0.8, 0.3,
                      0.3, 0.3), nrow = 2)
    B_er <-  matrix(c(0.5, 0.5,
                      0.5, 0.5), nrow = 2)

    # Core-periphery simulation.
    A_cp <- simulate_sbm2(n, B_cp, assignments)
    g_cp <- igraph::graph_from_adjacency_matrix(A_cp)
    # Simple random graph.
    A_er <- simulate_sbm2(n, B_er, assignments)
    g_er <- igraph::graph_from_adjacency_matrix(A_er)

    ## Embed both with ASE.
    dim <- 2
    X_cp <- lse(g_cp, dim)
    X_er <- lse(g_er, dim)

    ## Perform k-means clustering on embedded data.
    kmeans_cp <- kmeans(X_cp, 2)$cluster
    kmeans_er <- kmeans(X_er, 2)$cluster

    ## Check ARI of both clustering assignments.
    ari_cp <- mclust::adjustedRandIndex(kmeans_cp, assignments)
    ari_er <- mclust::adjustedRandIndex(kmeans_er, assignments)
    if (ari_cp > ari_er) { cp_is_better <- cp_is_better + 1 }
    else { er_is_better <- er_is_better + 1 }

  }

  ## We expect 2-Block SBM to have a higher value.
  expect_true( cp_is_better > er_is_better)

})

