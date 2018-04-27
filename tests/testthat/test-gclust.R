context("Select Number of Mixture Components")


test_that("Too low or non-integer 'K' value.", {

  # Define simple data matrix
  n <- 10
  d <- 5
  X <- matrix(rep(0, n*d), nrow = n)

  # Less than 1.
  K <- -1
  expect_error(gclust(X, K), "Input 'K' must be greater than or equal to 1.")

  # Not a number.
  K <- "string"
  expect_error(gclust(X, K), "Input 'K' must be an integer.")
  K <- matrix(c(1, 0 , 0 ,1), nrow = 2)
  expect_error(gclust(X, K), "Input 'K' must be an integer.")

})

test_that("'X' is not a proper matrix.", {

  # Make data frame and vector as incorrect matrices.
  n <- 10

  X <- data.frame(rep("string1", n), rep("string2", n))
  K <- 1
  expect_error(gclust(X, K), "Input 'X' is not a numeric matrix.")
  X <- rep(10, n)
  K <- 1
  expect_error(gclust(X, K), "Input 'X' is not a numeric matrix.")

})

test_that("General functionality.", {

  # Selects two clusters for mixture of two highly centered components.
  g <- 2
  n <- 50*g
  X1 <- rnorm(n/g, 0, 0.5)
  X2 <- rnorm(n/g, 5, 0.5)
  X <- as.matrix(c(X1, X2), nrow = n)
  K <- 5
  expect_equal(gclust(X), g)

  # Same test for 3 components.
  n <- 50*g
  X1 <- rnorm(n/g, 0, 0.5)
  X2 <- rnorm(n/g, 3, 0.5)
  X3 <- rnorm(n/g, 6, 0.5)
  X <- as.matrix(c(X1, X2, X3), nrow = n)
  K <- 5
  expect_equal(gclust(X), g)

})
