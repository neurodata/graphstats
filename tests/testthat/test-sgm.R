context("Seeded Graph Matching")

# Input Validation Tests
test_that("Input Validation for sgm (unordered)", {

  # Matrices
  seeds <- matrix(c(1, 1, 2, 2), nrow = 2)
  A <- "string"
  B <- matrix(c(0, 1, 1, 0), nrow = 2)
  expect_error(sgm(A, B, seeds), "Error: Input 'A' must be a matrix.")
  B <- "string"
  A <- matrix(c(0, 1, 1, 0), nrow = 2)
  expect_error(sgm(A, B, seeds), "Error: Input 'B' must be a matrix.")
  A <- matrix(rep(1, 3*2), nrow = 3)
  B <- matrix(c(0, 1, 1, 0), nrow = 2)
  expect_error(sgm(A, B, seeds), "Error: 'A' is not a square matrix.")
  B <- matrix(rep(1, 3*2), nrow = 2)
  A <- matrix(c(0, 1, 1, 0), nrow = 2)
  expect_error(sgm(A, B, seeds), "Error: 'B' is not a square matrix.")

  # Seeds
  A <- B <- matrix(c(0, 1, 1, 0), nrow = 2)
  seeds <- "string"
  expect_error(sgm(A, B, seeds), "Error: Input 'seeds' must be an m by 2 matrix of corresponding seed indices.")
  seeds <- diag(3)
  expect_error(sgm(A, B, seeds), "Error: Input 'seeds' must be an m by 2 matrix of corresponding seed indices.")
  seeds <- diag(2) + 0.5
  expect_error(sgm(A, B, seeds), "Error: Input 'seeds' must be an m by 2 matrix of corresponding seed indices.")
  seeds <- matrix(rep(1, 3*2), nrow = 3)
  expect_error(sgm(A, B, seeds), "Error: Number of seeds is greater than number of vertices in A.")


  # Set up for other tests.
  A <- B <- matrix(c(0, 1, 1, 0), nrow = 2)
  seeds <- matrix(c(1, 1, 2, 2), nrow = 2)

  # Hard/Soft
  hard <- 50
  expect_error(sgm(A, B, seeds, hard = hard), "Error: Input 'hard' must be a logical.")
  hard <- "string"
  expect_error(sgm(A, B, seeds, hard = hard), "Error: Input 'hard' must be a logical.")

  # Padding
  pad <- "string"
  expect_error(sgm(A, B, seeds, pad = pad), "Error: Input 'pad' must be a number.")

  # Iterations
  maxiter <- 1.5
  expect_error(sgm(A, B, seeds, maxiter = maxiter), "Error: Input 'maxiter' must be an integer.")
  maxiter <- -10
  expect_error(sgm(A, B, seeds, maxiter = maxiter), "Error: Iterations 'maxiter' is less than 0.")
  maxiter <- "string"
  expect_error(sgm(A, B, seeds, maxiter = maxiter), "Error: Input 'maxiter' must be an integer.")

})

test_that("Input Validation for sgm.ordered", {

  # TO DO.

})

# Test that we are correctly optimizing on edge disagreements.
test_that("End-to-end: Identical graph vs rho-SBM.", {

  # TO DO

})

# Test that we are correctly optimizing on edge disagreements.
test_that("End-to-end: rho-SBM vs ER graph.", {

  # TO DO.

})

