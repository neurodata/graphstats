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
test_that("End-to-end: Identical graph vs r-SBM.", {

  # Number of simulations.
  num_right <- 0
  num_wrong <- 0

  # Create graph.
  ns <- seq(50,200,10)
  p <- 0.5
  r <- 0.8
  h <- 2
  ell <- 2
  R <- 100
  gamma <- 0.01

  result <- lapply(ns, function(n) {
    # Simulate identical ER graphs
    # Choose 0.2 as the proportion of seeds in a graph
    m = 0.2 * n
    x = m + 1
    ga1 <- igraph::sample_sbm(n, p, n)
    cl <- igraph::clusters(ga1)
    ga1 <- igraph::induced.subgraph(ga1, which(cl$membership == which.max(cl$csize)))
    ga2 <- ga1

    # Permute the second.
    permute = c(1:m, sample(n-m)+m)
    ga2_p <- igraph::permute.vertices(ga2, permute)

    # Run VN to produce nominations for VOI x = m + 1.
    seeds <- as.matrix(cbind(1:m, 1:m), nrow = m)
    vn1 <- vnsgm(x,seeds,ga1,ga2_p,h,ell,R,gamma)
    id_matched = as.vector(which.max(vn1$P[as.character(x),]))

    # Count the number of vertex disagreements in nomination.
    if (permute[x] == id_matched) {
      num_right <- num_right + 1
    } else {
      num_wrong <- num_wrong + 1
    }
    return(list(num_right = num_right, num_wrong = num_wrong))
  })

  num_right <- sapply(result, function(res){res$num_right})
  num_wrong <- sapply(result, function(res) {res$num_wrong})

  # Test difference in disagreements via Wilcoxon Test. Caution: This is a hack.
  alpha <- 0.05
  pval <- wilcox.test(num_wrong, num_right, alt='less', exact=FALSE)$p.value
  expect_lt(pval, alpha)

})
