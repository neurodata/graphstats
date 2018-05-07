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
test_that("End-to-end testing.", {

  # Run two seperate Wilcoxon tests on a strong 2-block SBM and a simple random graph.
  num_sims <- 10
  dim <- 2
  test_vector <- rep(dim, num_sims)
  sbm_dimselected <- c()
  er_dimselected <- c()

  for (s in 1:num_sims) {

    ## Simulate  core-periphery SBM, and simple ER graph.
    set.seed(123)
    n <- 100
    num_class1 <- n/2

    # SBM Params
    num_class2 <- n - num_class1
    assignments <- c(rep(1, num_class1), rep(2, num_class2))
    B_sbm <- matrix(c(0.8, 0.2,
                      0.2, 0.8), nrow = 2)
    p <- 0.5
    B_er  <- matrix(c(p, p,
                      p, p), nrow = 2)

    # 2-block simulation.
    g_sbm <- igraph::sample_sbm(n, pref.matrix=B_sbm, block.sizes=c(num_class1, num_class2))
    # Simple random graph.
    g_er <- igraph::sample_sbm(n, pref.matrix=B_er, block.sizes=c(num_class1, num_class2))

    ## Embed both with ASE; get singular values from adjacency matrix;
    ## select dimenstion with dimselect.
    A_sbm <- igraph::as_adj(g_sbm)
    sigma_sbm <- rARPACK::svds(A_sbm,10)$d
    dim_sbm <- dimselect(sigma_sbm)[1]
    sbm_dimselected <- append(sbm_dimselected, dim_sbm)

    A_er <- igraph::as_adj(g_er)
    sigma_er <- rARPACK::svds(A_er,10)$d
    dim_er <- dimselect(sigma_er)[1]
    er_dimselected <- append(er_dimselected, dim_er)
  }

  p_val_sbm = suppressWarnings(wilcox.test(sbm_dimselected, test_vector, correct=FALSE, paired = TRUE)$p.value)
  p_val_er = suppressWarnings(wilcox.test(er_dimselected, test_vector, correct=FALSE, paired = TRUE)$p.value)

  ## We expect 2-Block SBM to have a higher p-value.
  expect_true( is.na(p_val_sbm) || p_val_sbm < p_val_er )
})

