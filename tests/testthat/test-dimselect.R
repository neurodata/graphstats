context("Automatic Dimensionality Selection")

# Input Tests

test_that("Out-of-range, or non-integer 'n' value.", {

  # Define standard deviation vector.
  sdev = c(100,99,98,50,49,48)
  leneig = length(sdev)

  # Less than 1, or greater than the number of vertices.
  n <- 0
  expect_error(gs.dim.select(sdev, n=n), "Input object 'n' must be in the appropriate interval.")
  n <- -10
  expect_error(gs.dim.select(sdev, n=n), "Input object 'n' must be in the appropriate interval.")
  n <- leneig + 1
  expect_error(gs.dim.select(sdev, n=n), "Input object 'n' must be in the appropriate interval.")

  # Not a single number.
  n <- "string"
  expect_error(gs.dim.select(sdev, n=n), "Input object 'n' is not a numeric value.")
  n <- sdev
  expect_error(gs.dim.select(sdev, n=n), "Input object 'n' is not a numeric value.")
  n <- matrix(c(1, 0, 0, 1), nrow = 2)
  expect_error(gs.dim.select(sdev, n=n), "Input object 'n' is not a numeric value.")
})

test_that("Incorrect input standard deviation vector 'dat'.", {

  # Not a graph or a matrix of any kind.
  sdev <- "string"
  n <- 1
  expect_error(gs.dim.select(sdev, n=n), "Input object 'X' is not a graph, a matrix/complex matrix or 2-D array, array, nor a one-dimensional numeric array.")

  # Matrix, but not a valid sdev vector.
  sdev <- matrix(c('a', 2, 3, 4, 5, 6))
  n <- 1
  expect_error(gs.dim.select(sdev, n=n), "Your input 'X' is a 1-D vector, array, or matrix, but has invalid entries and cannot be cast to numeric.")
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
    n <- 40
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
    set.seed(123)
    g_sbm <- igraph::sample_sbm(n, pref.matrix=B_sbm, block.sizes=c(num_class1, num_class2))
    # Simple random graph.
    g_er <- igraph::sample_sbm(n, pref.matrix=B_er, block.sizes=c(num_class1, num_class2))

    ## Embed both with ASE; get singular values from adjacency matrix;
    ## select dimenstion with dimselect.
    A_sbm <- igraph::as_adj(g_sbm)
    sigma_sbm <- rARPACK::svds(A_sbm,10)$d
    dim_sbm <- gs.dim.select(sigma_sbm)$elbow[1]
    sbm_dimselected <- append(sbm_dimselected, dim_sbm)

    A_er <- igraph::as_adj(g_er)
    sigma_er <- rARPACK::svds(A_er,10)$d
    dim_er <- gs.dim.select(sigma_er)$elbow[1]
    er_dimselected <- append(er_dimselected, dim_er)
  }

  p_val = wilcox.test(er_dimselected,sbm_dimselected, alt = 'less', exact = FALSE)$p.value
  ## We expect 2-Block SBM to have a higher p-value.
  expect_true(p_val < 0.05 )
})

