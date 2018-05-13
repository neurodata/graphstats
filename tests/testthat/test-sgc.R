context("Spectral Graph Clustering")

# Deterin OS to suppress output.
if (.Platform$OS.type == 'windows') {
  void_output = 'NUL'
} else if ((.Platform$OS.type == 'unix')) {
  void_output = "\\dev\\null"
}

capture.output( {

test_that("Input Validation Tests.", {

  # Graph
  g <- "string"
  expect_error(sgc(g), "Error: Input object 'g' must be an igraph object.")
  g <- matrix(rep(0, 5*5), nrow = 5)
  expect_error(sgc(g), "Error: Input object 'g' must be an igraph object.")

  # Test Parameters for graph.
  n <- 10
  p <- 0.5
  g <- igraph::sample_sbm(n, p, n)

  # Dmax
  dmax <- "string"
  expect_error(sgc(g, dmax=dmax), "Error: Input 'dmax' must be an integer and >=1.")
  dmax <- 0
  expect_error(sgc(g, dmax=dmax), "Error: Input 'dmax' must be an integer and >=1.")

  # Elbow
  elb <- "string"
  expect_error(sgc(g, elb=elb), "Error: Input 'elb' must be an integer and >=1.")
  elb <- 0
  expect_error(sgc(g, elb=elb), "Error: Input 'elb' must be an integer and >=1.")

  # More elbows than dimensions.
  dmax <- 3
  elb <- dmax + 1
  expect_error(sgc(g, dmax=dmax, elb=elb),
               "Error: Number of elbows 'elb' is greater than maximum dimension 'dmax'.")

  # Absolute value, then get elbow.
  abs <- 5
  expect_error(sgc(g, abs=abs), "Error: Input 'abs' must be a logical.")
  abs <- "string"
  expect_error(sgc(g, abs=abs), "Error: Input 'abs' must be a logical.")

  # Largest Connected Component
  lcc <- 5
  expect_error(sgc(g, lcc=lcc), "Error: Input 'lcc' must be a logical.")
  lcc <- "string"
  expect_error(sgc(g, lcc=lcc), "Error: Input 'lcc' must be a logical.")

  # Embedding
  embed <- "string"
  expect_error(sgc(g, embed = embed), "Error: 'embed' must be a string equal to 'ASE' or 'LSE'.")
  embed <- 1
  expect_error(sgc(g, embed = embed), "Error: 'embed' must be a string equal to 'ASE' or 'LSE'.")

  # Clustering
  clustering <- "string"
  expect_error(sgc(g, clustering = clustering), "Error: 'clustering' must be a string equal to 'GMM' or 'Kmeans'.")
  clustering <- 1
  expect_error(sgc(g, clustering = clustering), "Error: 'clustering' must be a string equal to 'GMM' or 'Kmeans'.")

  # Kmax
  Kmax <- "string"
  expect_error(sgc(g, Kmax=Kmax), "Error: Input 'Kmax' must be an integer and >=1.")
  Kmax <- 0
  expect_error(sgc(g, Kmax=Kmax), "Error: Input 'Kmax' must be an integer and >=1.")

  # Weight
  weight <- "string"
  expect_error(sgc(g, weight = weight), "Error: 'weight' must be a string equal to 'ptr', 'binary', or 'raw'.")
  weight <- 1
  expect_error(sgc(g, weight = weight), "Error: 'weight' must be a string equal to 'ptr', 'binary', or 'raw'.")

  # Verbose
  verbose <- 5
  expect_error(sgc(g, verbose=verbose), "Error: Input 'verbose' must be a logical.")
  verbose <- "string"
  expect_error(sgc(g, verbose=verbose), "Error: Input 'verbose' must be a logical.")

  # Plotting
  doplot <- 5
  expect_error(sgc(g, doplot=doplot), "Error: Input 'doplot' must be a logical.")
  doplot <- "string"
  expect_error(sgc(g, doplot=doplot), "Error: Input 'doplot' must be a logical.")

})

test_that("Small K, separable cases.", {

  # Test that sgc regonizes the clusters correctly.

  set.seed(456)
  num_sims <- 10
  num_right <- 0
  num_wrong <- 0
  g <- 2
  n <- 50*g

  for (s in 1:num_sims) {
    # Separates data into two clusters.
    B <- matrix(c(0.8, 0.3,
                  0.3, 0.8), nrow = 2)
    block_sizes <- c(n/g, n/g)
    assignments <- rep(c(1,2), block_sizes)
    G <- igraph::sample_sbm(n, B, block_sizes)
    predicted <- sgc(G, verbose = FALSE)$Y
    ari <- mclust::adjustedRandIndex(predicted, assignments)
    if (ari == 1) {
      num_right <- num_right + 1
    } else {
      num_wrong <- num_wrong + 1
    }
  }
  expect_true(num_right > num_wrong)

  # Same test for 3 components.
  g <- 3
  num_right <- 0
  num_wrong <- 0
  n <- 50*g

  for (s in 1:num_sims) {
    # Separates data into three clusters.
    B <- matrix(c(0.8, 0.3, 0.1,
                  0.3, 0.8, 0.3,
                  0.1, 0.3, 0.8), nrow = 3)
    block_sizes <- c(n/g, n/g, n/g)
    assignments <- rep(c(1,2,3), block_sizes)
    G <- igraph::sample_sbm(n, B, block_sizes)
    predicted <- sgc(G, verbose = FALSE)$Y
    ari <- mclust::adjustedRandIndex(predicted, assignments)
    if (ari == 1) {
      num_right <- num_right + 1
    } else {
      num_wrong <- num_wrong + 1
    }
  }
  expect_true(num_right > num_wrong)

})

test_that("Performs better on separable 2-SBM than simple random graphs.", {

  # Number of simulations.
  num_sims <- 20
  set.seed(123)

  # Create two graphs - a simple ER and a separable 2-SBM.
  n <- 40
  p <- 0.5
  num_class1 <- n/2
  num_class2 <- n - num_class1
  assignments <- c(rep(1, num_class1), rep(2, num_class2))
  B <- matrix(c(0.8, 0.3,
                0.3, 0.8), nrow = 2)

  result <- lapply(1:num_sims, function(i) {

    # Simulation.
    g1 <- igraph::sample_sbm(n, pref.matrix=p, block.sizes=n)
    g2 <- igraph::sample_sbm(n, pref.matrix=B, block.sizes=c(num_class1, num_class2))

    # Run spectral grah clustering on both.
    SGC1 <- sgc(g1, verbose = FALSE)
    SGC2 <- sgc(g2, verbose = FALSE)

    # Calculate cluster quality between SGC on either graph.
    ari1 <- mclust::adjustedRandIndex(SGC1$Y, assignments)
    ari2 <- mclust::adjustedRandIndex(SGC2$Y, assignments)
    return(list(ari1 = ari1, ari2 = ari2))
  })

  # Split results into separate vectors.
  result_er  <- sapply(result, function(res){res$ari1})
  result_sbm <- sapply(result, function(res) {res$ari2})

  # Test difference in disagreements via Wilcoxon Test. Caution: This is a hack.
  alpha <- 0.05
  pval <- wilcox.test(result_er, result_sbm, alt='less', exact=FALSE)$p.value
  expect_lt(pval, alpha)

})

}, file = void_output)
