context("Vertex Nomination via Seeded Graph Matching")

# Input Validation Tests
test_that("Input Validation for vnsgm (unordered)", {

  # x
  seeds <- matrix(c(1, 1), nrow = 1)
  x <- "string"
  A <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  B <- A
  g1 <- igraph::graph_from_adjacency_matrix(A)
  g2 <- igraph::graph_from_adjacency_matrix(B)
  h <- 1
  ell <- 1
  R <- 1
  gamma <- 0.01
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma), "Error: Input 'x' must be a numeric vector.")

  # Graphs
  x <- 3
  g1 <- "string"
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma), "Input object 'g1' is not an igraph object.")

  g1 <- g2
  g2 <- "string"
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma), "Input object 'g2' is not an igraph object.")

  # Seeds
  g1 <- igraph::graph_from_adjacency_matrix(A)
  g2 <- igraph::graph_from_adjacency_matrix(B)
  seeds <- "string"
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma), "Error: Input 'seeds' must be an m by 2 matrix of corresponding seed indices.")
  seeds <- diag(3)
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma), "Error: Input 'seeds' must be an m by 2 matrix of corresponding seed indices.")
  seeds <- diag(2) + 0.5
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma), "Error: Input 'seeds' must be an m by 2 matrix of corresponding seed indices.")
  seeds <- matrix(rep(1, 4*2), nrow = 4)
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma), "Error: Number of seeds is greater than number of vertices in g1.")


  # Set up for other tests.
  g1 <- graph_from_adjacency_matrix(A)
  g2 <- graph_from_adjacency_matrix(B)
  seeds <- matrix(c(1, 1), nrow = 1)

  # h
  h <- "string"
  expect_error(vnsgm(x,seeds,g1,g2,h = h,ell,R,gamma), "Error: Input 'h' must be a positive number.")

  # ell
  h <- 1
  ell <- "string"
  expect_error(vnsgm(x,seeds,g1,g2,h,ell = ell,R,gamma), "Error: Input 'ell' must be a positive number.")

  # R
  ell <- 1
  R <- "string"
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R = R,gamma), "Error: Input 'R' must be a number.")

  # R
  R <- 1
  gamma <- "string"
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma=gamma), "Error: Input 'gamma' must be a number.")
  gamma <- -1
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma=gamma), "Error: Input 'gamma' must be between 0 and 1.")


  # Padding
  gamma <- 0.01
  pad <- "string"
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma,pad = pad), "Error: Input 'pad' must be a number.")

  # Iterations
  x <- 2
  seeds <- matrix(c(1, 1), nrow = 1)
  maxiter <- -10
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma,maxiter = maxiter), "Error: Input 'maxiter' must be a positive number.")
  maxiter <- "string"
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma,maxiter = maxiter), "Error: Input 'maxiter' must be a positive number.")

  #Verbosity
  maxiter <- 10
  verbose <- 50
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma,verbose = verbose), "Error: Input 'verbose' must be a logical.")
  verbose <- "string"
  expect_error(vnsgm(x,seeds,g1,g2,h,ell,R,gamma,verbose = verbose), "Error: Input 'verbose' must be a logical.")

})


test_that("Input Validation for vnsgm.ordered", {

  # x
  s <- 1
  x <- "string"
  A <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  B <- A
  g1 <- igraph::graph_from_adjacency_matrix(A)
  g2 <- igraph::graph_from_adjacency_matrix(B)
  h <- 1
  ell <- 1
  R <- 1
  gamma <- 0.01
  expect_error(vnsgm.ordered(x,s,g1,g2,h,ell,R,gamma), "Error: Input 'x' must be a numeric vector.")

  # Graphs
  x <- 1
  g1 <- "string"
  expect_error(vnsgm.ordered(x,s,g1,g2,h,ell,R,gamma), "Input object 'g1' is not an igraph object.")

  g1 <- g2
  g2 <- "string"
  expect_error(vnsgm.ordered(x,s,g1,g2,h,ell,R,gamma), "Input object 'g2' is not an igraph object.")


  # Set up for other tests.
  g1 <- graph_from_adjacency_matrix(A)
  g2 <- graph_from_adjacency_matrix(B)

  # s
  s <- "string"
  expect_error(vnsgm.ordered(x,s=s,g1,g2,h,ell,R,gamma), "Error: Input 's' must be a positive number.")

  # h
  s <- 1
  h <- "string"
  expect_error(vnsgm.ordered(x,s,g1,g2,h = h,ell,R,gamma), "Error: Input 'h' must be a positive number.")

  # ell
  h <- 1
  ell <- "string"
  expect_error(vnsgm.ordered(x,s,g1,g2,h,ell = ell,R,gamma), "Error: Input 'ell' must be a positive number.")

  # R
  ell <- 1
  R <- "string"
  expect_error(vnsgm.ordered(x,s,g1,g2,h,ell,R = R,gamma), "Error: Input 'R' must be a number.")

  # R
  R <- 1
  gamma <- "string"
  expect_error(vnsgm.ordered(x,s,g1,g2,h,ell,R,gamma=gamma), "Error: Input 'gamma' must be a number.")
  gamma <- -1
  expect_error(vnsgm.ordered(x,s,g1,g2,h,ell,R,gamma=gamma), "Error: Input 'gamma' must be between 0 and 1.")


  # Padding
  gamma <- 0.01
  pad <- "string"
  expect_error(vnsgm.ordered(x,s,g1,g2,h,ell,R,gamma,pad = pad), "Error: Input 'pad' must be a number.")

  # Iterations
  pad <- 0
  maxiter <- -10
  expect_error(vnsgm.ordered(x,s,g1,g2,h,ell,R,gamma,maxiter = maxiter), "Error: Input 'maxiter' must be a positive number")
  maxiter <- "string"
  expect_error(vnsgm.ordered(x,s,g1,g2,h,ell,R,gamma,maxiter = maxiter), "Error: Input 'maxiter' must be a positive number.")

  #Verbosity
  maxiter <- 10
  verbose <- 50
  expect_error(vnsgm.ordered(x,s,g1,g2,h,ell,R,gamma,verbose = verbose), "Error: Input 'verbose' must be a logical.")
  verbose <- "string"
  expect_error(vnsgm.ordered(x,s,g1,g2,h,ell,R,gamma,verbose = verbose), "Error: Input 'verbose' must be a logical.")

})


# Test that we correctly nominate vertices.
# vnsgm should perform better on a identical graphs than a correlated SBM.
test_that("End-to-end: Identical graph vs r-SBM.", {

  # Number of simulations.
  num_sims <- 20

  # Create two graphs sets of graphs.
  n <- 10
  m <- 3
  p <- 0.5
  r <- 0.8
  x = 4
  h = 2
  ell = 2
  R = 100
  gamma = 0.01

  result <- lapply(1:num_sims, function(i) {
    # One is a pair of identical graphs
    ga1 <- gb1 <- igraph::sample_sbm(n, p, n)
    A1 <- B1 <- as.matrix(igraph::as_adj(ga1))

    # The other is a pair of r-SBM distributed graphs.
    X <- matrix(rep(c(0.5, 0.5), n), nrow = n)
    corr_graphs <- rdpg.sample.correlated(X, r)
    ga2 = corr_graphs[[1]]
    gb2 = corr_graphs[[2]]
    A2 <- as.matrix(igraph::as_adj(ga2))
    B2 <- as.matrix(igraph::as_adj(gb2))

    # Permute the second of each pair.
    permute = c(1:m, sample(n-m)+m)
    B1_p <- permute.vertices(gb1, permute)
    B2_p <- permute.vertices(gb2, permute)

    # Run seeded graph matching to produce permutation matrices, and apply them.
    seeds <- as.matrix(cbind(1:m, 1:m), nrow = m)
    vn1 <- vnsgm(x,seeds,ga1,ga2,h,ell,R,gamma)
    vn2 <- vnsgm(x,seeds,gb1,gb2,h,ell,R,gamma)
    gb1_matched = as.vector(which.max(vn1$P[as.character(x),]))
    gb2_matched = as.vector(which.max(vn2$P[as.character(x),]))


    # Count the number of vertex disagreements between the matched graphs.
    E1 <- abs(permute[x] - gb1_matched)
    E2 <- abs(permute[x] - gb2_matched)
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
