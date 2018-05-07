context("Nonparametric two-sample testing")

# Input Tests

test_that("Too low, too high, or non-integer 'dim' value.", {

  # Define simple 2-vertex graph.
  n <- 2
  A <- matrix(c(0, 1, 1, 0), nrow = n)
  g1 <- igraph::graph_from_adjacency_matrix(A)
  g2 <- g1

  # Less than 1, or greater than the number of vertices.
  dim <- 0
  expect_error(nonpar(g1, g2, dim = dim), "Number of dimensions 'dim' is less than 1.")
  dim <- -10
  expect_error(nonpar(g1, g2, dim = dim), "Number of dimensions 'dim' is less than 1.")
  dim <- n + 1
  expect_error(nonpar(g1, g2, dim = dim), "Num. Embedded dimensions 'dim' is greater than number of vertices.")

  # Not a single number.
  dim <- "string"
  expect_error(ase(g, dim), "Input 'dim' is not a number.")
  dim <- g
  expect_error(ase(g, dim), "Input 'dim' has length > 1.")
  dim <- matrix(c(1, 0, 0, 1), nrow = 2)
  expect_error(ase(g, dim), "Input 'dim' has length > 1.")
  dim <- c(5, 6, 7)
  expect_error(ase(g, dim), "Input 'dim' has length > 1.")

})

test_that("Incorrect input graph 'g'.", {

  # Not a graph or a matrix of any kind.
  g <- "string"
  dim <- 1
  expect_error(ase(g, dim), "Input object 'g' is not an igraph object.")

  # 'g' contains multiple graphs.
  A1 <- matrix(c(1, 0, 0, 1), nrow = 2)
  A2 <- matrix(c(1, 0, 0, 1), nrow = 2)
  A <- c(A1, A2)
  dim <- 1
  expect_error(ase(A, dim), "Input object 'g' is not an igraph object.")

  # Matrix, but not a valid adjacency (square) matrix.
  A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  dim <- 1
  expect_error(ase(A, dim), "Non-square matrix, Non-square matrix")
})

test_that("Incorrect sigma value.", {

  # Not a graph or a matrix of any kind.
  Xhat1 <- matrix(c(0, 1, 1,
                    1, 0, 1,
                    1, 1, 0),
                    nrow = 3)
  Xhat2 <- matrix(c(0, 1, 1,
                    1, 0, 1,
                    1, 1, 0),
                    nrow = 3)
  sigma = igraph::graph_from_adjacency_matrix(Xhat2)
  expect_error(nonpar(Xhat1, Xhat2, sigma), "Input object 'sigma' is not a numeric value.")

  sigma = Xhat1
  expect_error(nonpar(Xhat1, Xhat2, sigma), "Input object 'sigma' is not a numeric value.")

  sigma = -5
  expect_error(nonpar(Xhat1, Xhat2, sigma), "Input object 'sigma' must be positive.")
})
