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

# General Functionality.
test_that("Correct output for various matrices.", {

  # Produce random matrix, and embed to 2 dims.
  n <- 7
  dim <- 3
  A <- matrix(rep(0, n*n), nrow = n)
  for (i in 1:n) {
    for (j in 1:(i-1)) {
      A[i, j] <- rbinom(1, 1, 0.5)
      A[j, i] <- A[i, j]
    }
  }
  #print(A)

  # Compute SVD
  SVD <- svd(A)
  U <- SVD$u
  D <- SVD$d
  V <- SVD$v

  W_test <- U[,1:dim] %*% diag(sqrt(D[1:dim]))
  W <- lse(A, dim)
  #print(D)
  #print(U_test)
  #print(W)
  #print(W - U_test)

})

