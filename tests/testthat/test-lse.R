context("Laplacian Spectral Embedding")

test_that("Too low, too high, or non-integer 'dim' value.", {

  # Define simple 2-vertex graph.
  n <- 2
  A <- matrix(c(0, 1, 1, 0), nrow = n)
  g <- igraph::graph_from_adjacency_matrix(A)

  # Less than 1, or greater than the number of vertices.
  dim <- 0
  expect_error(gs.embed.lse(g, dim), "The number of embedding dimensions 'k' < 1.")
  dim <- -10
  expect_error(gs.embed.lse(g, dim), "The number of embedding dimensions 'k' < 1.")
  dim <- n + 1
  expect_error(gs.embed.lse(g, dim), "The number of embedding dimensions 'k' is greater than number of vertices.")

  # Not a single number.
  dim <- "string"
  expect_error(gs.embed.lse(g, dim), "The number of embedding dimensions 'k' is not a number.")
  dim <- g
  expect_error(gs.embed.lse(g, dim), "The number of embedding dimensions 'k' has length > 1.")
  dim <- matrix(c(1, 0, 0, 1), nrow = 2)
  expect_error(gs.embed.lse(g, dim), "The number of embedding dimensions 'k' has length > 1.")
  dim <- c(5, 6, 7)
  expect_error(gs.embed.lse(g, dim), "The number of embedding dimensions 'k' has length > 1.")

})

test_that("Incorrect input graph 'g'.", {

  # Not a graph or a matrix of any kind.
  g <- "string"
  dim <- 1
  expect_error(gs.embed.lse(g, dim), "Input object 'g' is not an igraph object.")

  # 'g' contains multiple graphs.
  A1 <- matrix(c(1, 0, 0, 1), nrow = 2)
  A2 <- matrix(c(1, 0, 0, 1), nrow = 2)
  A <- c(A1, A2)
  dim <- 1
  expect_error(gs.embed.lse(A, dim), "Input object 'g' is not an igraph object.")

  # Matrix, but not a valid adjacency (square) matrix.
  A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  dim <- 1
  expect_error(gs.embed.lse(A, dim), "Non-square matrix, Non-square matrix")
})

# General Functionality.
test_that("End-to-end testing.", {

  # Number of simulations. Count how many times latent block assignments are recovered
  # via kmeans clustering of ASE from a core-periphery 2-SBM and simple random graph.
  num_sims <- 10
  cp_is_better <- 0
  er_is_better <- 0
  set.seed(123)

  for (s in 1:num_sims) {

    ## Simulate  core-periphery SBM, and simple ER graph.
    n <- 100
    num_class1 <- n/2

    # SBM Params
    num_class2 <- n - num_class1
    assignments <- c(rep(1, num_class1), rep(2, num_class2))
    B_cp <- matrix(c(0.8, 0.3,
                     0.3, 0.3), nrow = 2)
    p <- 0.5
    B_er <- matrix(c(p, p,
                     p, p), nrow = 2)

    # Core-periphery simulation.
    g_cp <- igraph::sample_sbm(n, pref.matrix=B_cp, block.sizes=c(num_class1, num_class2))
    # Simple random graph.
    g_er <- igraph::sample_sbm(n, pref.matrix=B_er, block.sizes=c(num_class1, num_class2))

    ## Embed both with ASE.
    dim <- 2
    X_cp <- gs.embed.lse(g_cp, dim)
    X_er <- gs.embed.lse(g_er, dim)

    ## Perform k-means clustering on embedded data.
    kmeans_cp <- kmeans(X_cp$X, 2)$cluster
    kmeans_er <- kmeans(X_er$X, 2)$cluster

    ## Check ARI of both clustering assignments.
    ari_cp <- mclust::adjustedRandIndex(kmeans_cp, assignments)
    ari_er <- mclust::adjustedRandIndex(kmeans_er, assignments)
    if (ari_cp > ari_er) { cp_is_better <- cp_is_better + 1 }
    else { er_is_better <- er_is_better + 1 }

  }

  ## We expect 2-Block SBM to have a higher value.
  expect_true( cp_is_better > er_is_better)

})

