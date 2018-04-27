context("Automatic Dimensionality Selection")

# Input Tests

test_that("Out-of-range, or non-integer 'n' value.", {

  # Define standard deviation vector.
  sdev = c(100,99,98,50,49,48)
  leneig = length(sdev)

  # Less than 1, or greater than the number of vertices.
  n <- 0
  expect_error(dimselect(sdev, n), "Input object 'n' must be in the appropriate interval.")
  n <- -10
  expect_error(dimselect(sdev, n), "Input object 'n' must be in the appropriate interval.")
  n <- leneig + 1
  expect_error(dimselect(sdev, n), "Input object 'n' must be in the appropriate interval.")

  # Not a single number.
  n <- "string"
  expect_error(dimselect(sdev, n), "Input object 'n' is not a numeric value.")
  n <- sdev
  expect_error(dimselect(sdev, n), "Input object 'n' is not a numeric value.")
  n <- matrix(c(1, 0, 0, 1), nrow = 2)
  expect_error(dimselect(sdev, n), "Input object 'n' is not a numeric value.")
})

test_that("Incorrect input standard deviation vector 'dat'.", {

  # Not a graph or a matrix of any kind.
  sdev <- "string"
  n <- 1
  expect_error(dimselect(sdev, n), "Input object 'sdev' is not a one-dimensional numeric array.")

  # 'sdev' is a matrx with more than one row.
  A1 <- matrix(c(1, 0, 0, 1), nrow = 2)
  A2 <- matrix(c(1, 0, 0, 1), nrow = 2)
  sdev <- cbind(A1, A2)
  n <- 1
  expect_error(dimselect(sdev, n), "Input object 'sdev' is not a one-dimensional numeric array.")

  # Matrix, but not a valid sdev vector.
  sdev <- matrix(c('1', 2, 3, 4, 5, 6))
  n <- 1
  expect_error(dimselect(sdev, n), "Input object 'sdev' contains non-numeric values.")
})

# General Functionality.

test_that("Correct output for various matrices.", {

  # Define 3 x 3 matrix (triangle graph), and check for correct output.
  n <- 2
  A <- c(100, 99, 98, 50, 49, 48, 10, 9, 8)
  true_elbow = c(3, 6)
  dim = dimselect(A,n)
  expect_equal(dim, true_elbow)
})


