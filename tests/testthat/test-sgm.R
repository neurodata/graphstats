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

  #function(A,B,m,start,pad,maxiter,LAP,verbose)


  # Matrices
  start <- diag(5)
  m <- 5

  A <- "string"
  B <- matrix(c(0, 1, 1, 0), nrow = 2)
  expect_error(sgm.ordered(A, B, m, start), "Error: Input 'A' must be a matrix.")
  B <- "string"
  A <- matrix(c(0, 1, 1, 0), nrow = 2)
  expect_error(sgm.ordered(A, B, m, start), "Error: Input 'B' must be a matrix.")
  A <- matrix(rep(1, 3*2), nrow = 3)
  B <- matrix(c(0, 1, 1, 0), nrow = 2)
  expect_error(sgm.ordered(A, B, m, start), "Error: 'A' is not a square matrix.")
  B <- matrix(rep(1, 3*2), nrow = 2)
  A <- matrix(c(0, 1, 1, 0), nrow = 2)
  expect_error(sgm.ordered(A, B, m, start), "Error: 'B' is not a square matrix.")

  # Correct matrix format.
  n <- 10
  p <- 0.5
  A <- B <- igraph::as_adj(igraph::sample_sbm(n, p, n))
  start <- diag(5)

  # Number of Seeds
  # 3 + 5 = 8 != 10 = n
  expect_error(sgm.ordered(A, B, 3, start), "Error: Input 'start' must be a square, n-m by n-m matrix.")
  m <- "string"
  expect_error(sgm.ordered(A, B, m, start), "Error: Input 'm', number of seeds, must be an integer and >=0.")
  m <- -10
  expect_error(sgm.ordered(A, B, m, start), "Error: Input 'm', number of seeds, must be an integer and >=0.")

  # Correct number of seeds.
  m <- 5

  # Start
  start <- "matrix???"
  expect_error(sgm.ordered(A, B, m, start), "Error: Input 'start' must be a matrix.")
  start <- matrix(rep(1, m*n), nrow = m) # Non-square.
  expect_error(sgm.ordered(A, B, m, start), "Error: Input 'start' must be a square, n-m by n-m matrix.")
  start <- diag((m+n)/2) # Square but incorrect size.
  expect_error(sgm.ordered(A, B, m, start), "Error: Input 'start' must be a square, n-m by n-m matrix.")

  # Correct format for start (for other tests).
  start <- diag(n-m)

  # Padding
  pad <- "string"
  expect_error(sgm.ordered(A, B, m, start, pad = pad), "Error: Input 'pad' must be a number.")

  # Iterations
  maxiter <- 1.5
  expect_error(sgm.ordered(A, B, m, start, maxiter = maxiter), "Error: Input 'maxiter' must be an integer.")
  maxiter <- -10
  expect_error(sgm.ordered(A, B, m, start, maxiter = maxiter), "Error: Iterations 'maxiter' is less than 0.")
  maxiter <- "string"
  expect_error(sgm.ordered(A, B, m, start, maxiter = maxiter), "Error: Input 'maxiter' must be an integer.")

  # LAP
  LAP <- "string"
  expect_error(sgm.ordered(A, B, m, start, LAP = LAP), "Error: LAP must be a string equal to 'exact' or 'approx'.")
  LAP <- 1
  expect_error(sgm.ordered(A, B, m, start, LAP = LAP), "Error: LAP must be a string equal to 'exact' or 'approx'.")

  # Verbose
  verbose <- 5
  expect_error(sgm.ordered(A, B, m, start, verbose = verbose), "Error: Input 'verbose' must be a logical.")
  verbose <- "string"
  expect_error(sgm.ordered(A, B, m, start, verbose = verbose), "Error: Input 'verbose' must be a logical.")

})

# Test that we are correctly optimizing on edge disagreements.
test_that("End-to-end: Identical graph vs r-SBM.", {

  # Number of simulations.
  num_sims <- 20

  # Create two graphs sets of graphs.
  n <- 30
  m <- 5
  p <- 0.5
  r <- 0.8

  result <- lapply(1:num_sims, function(i) {
    # One is a pair of identical graphs
    A1 <- B1 <- as.matrix(igraph::as_adj(igraph::sample_sbm(n, p, n)))

    # The other is a pair of r-SBM distributed graphs.
    X <- matrix(rep(c(0.5, 0.5), n), nrow = n)
    corr_graphs <- rdpg.sample.correlated(X, r)
    A2 <- as.matrix(igraph::as_adj(corr_graphs[[1]]))
    B2 <- as.matrix(igraph::as_adj(corr_graphs[[2]]))

    # Permute the second of each pair.
    B1_p <- B1[1:m,sample(n-m)+m]
    B2_p <- B2[1:m,sample(n-m)+m]

    # Run seeded graph matching to produce permutation matrices, and apply them.
    seeds <- as.matrix(cbind(1:m, 1:m), nrow = m)
    P1 <- sgm(A1, B1, seeds)
    P2 <- sgm(A2, B2, seeds)
    B1_matched <- P1 %*% B1 %*% t(P1)
    B2_matched <- P2 %*% B2 %*% t(P2)

    # Count the number of edge disagreements between the matched graphs.
    E1 <- sum(abs(A1 - B1_matched))
    E2 <- sum(abs(A2 - B2_matched))
    return(list(E1 = E1, E2 = E2))
  })

  # Split results into separate vectors.
  result_identical <- sapply(result, function(res){res$E1})
  result_sbm <- sapply(result, function(res) {res$E2})

  # Test difference in disagreements via Wilcoxon Test. Caution: This is a hack.
  alpha <- 0.05
  pval <- wilcox.test(result_identical, result_sbm, alt='less', exact=FALSE)$p.value
  expect_lt(pval, alpha)

})

# Test that we are correctly optimizing on edge disagreements.
test_that("End-to-end: rho-SBM vs ER graph.", {

  # Number of simulations.
  num_sims <- 20

  # Create two graphs sets of graphs.
  n <- 30
  m <- 5
  p <- 0.5
  r <- 0.8

  result <- lapply(1:num_sims, function(i) {

    # One is a pair of r-SBM distributed graphs.
    X <- matrix(rep(c(0.5, 0.5), n), nrow = n)
    corr_graphs <- rdpg.sample.correlated(X, r)
    A1 <- as.matrix(igraph::as_adj(corr_graphs[[1]]))
    B1 <- as.matrix(igraph::as_adj(corr_graphs[[2]]))

    # The other is a pair os separately sampled simple random graphs.
    A2 <- as.matrix(igraph::as_adj(igraph::sample_sbm(n, p, n)))
    B2 <- as.matrix(igraph::as_adj(igraph::sample_sbm(n, p, n)))

    # Permute the second of each pair.
    B1_p <- B1[1:m,sample(n-m)+m]
    B2_p <- B2[1:m,sample(n-m)+m]

    # Run seeded graph matching to produce permutation matrices, and apply them.
    seeds <- as.matrix(cbind(1:m, 1:m), nrow = m)
    P1 <- sgm(A1, B1, seeds)
    P2 <- sgm(A2, B2, seeds)
    B1_matched <- P1 %*% B1 %*% t(P1)
    B2_matched <- P2 %*% B2 %*% t(P2)

    # Count the number of edge disagreements between the matched graphs.
    E1 <- sum(abs(A1 - B1_matched))
    E2 <- sum(abs(A2 - B2_matched))
    return(list(E1 = E1, E2 = E2))
  })

  # Split results into separate vectors.
  result_sbm <- sapply(result, function(res){res$E1})
  result_er <- sapply(result, function(res) {res$E2})

  # Test difference in disagreements via Wilcoxon Test. Caution: This is a hack.
  alpha <- 0.05
  pval <- wilcox.test(result_sbm, result_er, alt='less', exact=FALSE)$p.value
  expect_lt(pval, alpha)

})

