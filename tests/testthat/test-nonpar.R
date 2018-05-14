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
  expect_error(nonpar(g1, g2, dim = dim), "Input 'dim' is not a number.")
  dim <- g1
  expect_error(nonpar(g1, g2, dim = dim), "Input 'dim' is not a number.")
  dim <- matrix(c(1, 0, 0, 1), nrow = 2)
  expect_error(nonpar(g1, g2, dim = dim), "Input 'dim' is not a number.")
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
  n = 2
  A <- matrix(c(0, 1, 1, 0), nrow = n)
  g1 <- igraph::graph_from_adjacency_matrix(A)
  g2 <- g1
  sigma = g1
  expect_error(nonpar(g1, g2, sigma = sigma), "Input object 'sigma' is not a numeric value.")

  sigma = A
  expect_error(nonpar(g1, g2, sigma = sigma), "Input object 'sigma' is not a numeric value.")
})


# General Functionality.
test_that("End-to-end testing.", {

  # Run two seperate Wilcoxon tests on a strong 2-block SBM and a simple random graph.
  num_sims <- 10
  dim <- 2
  test.vector <- rep(0, num_sims)
  test.stat <- c()

  for (s in 1:num_sims) {

    ## Simulate  core-periphery SBM, and simple ER graph.
    set.seed(123)
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
    g_sbm <- igraph::sample_sbm(n, pref.matrix=B_sbm, block.sizes=c(num_class1, num_class2))
    # Simple random graph.
    g_er <- igraph::sample_sbm(n, pref.matrix=B_er, block.sizes=c(num_class1, num_class2))
    invisible(capture.output(np <- nonpar(g_sbm, g_er, bootstrap_sample = 50)))
    test.stat <- append(test.stat, np$p_value)
  }

  p_val = wilcox.test(test.stat,test.vector, alt = 'less', exact = FALSE)$p.value
  ## We expect 2-Block SBM to have a higher p-value.
  expect_true(1 - p_val < 0.05 )
})



