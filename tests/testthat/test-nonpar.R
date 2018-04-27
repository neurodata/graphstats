context("Nonparametric two-sample testing")

# Input Tests

test_that("Incorrect Xhat/Yhat data type.", {

  # Less than 1, or greater than the number of vertices.
  Xhat1 <- 0
  Xhat2 <- matrix(c(0, 1, 1,
                    1, 0, 1,
                    1, 1, 0),
                    nrow = 3)
  g = igraph::graph_from_adjacency_matrix(Xhat2)
  sigma = 0.5
  expect_error(nonpar(Xhat1, Xhat2, sigma), "Input object 'Xhat1' is not a matrix.")

  Xhat1 <- "string"
  expect_error(nonpar(Xhat1, Xhat2, sigma), "Input object 'Xhat1' is not a matrix.")
  Xhat1 <- g
  expect_error(nonpar(Xhat2, Xhat1, sigma), "Input object 'Xhat2' is not a matrix.")

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
