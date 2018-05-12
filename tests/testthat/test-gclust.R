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

test_that("Small K, separable cases.", {

  # Test that gclust regonizes the true number of clusters
  # when they are highly separated.

  set.seed(123)
  num_sims <- 10
  num_right <- 0
  num_wrong <- 0
  g <- 2
  n <- 50*g

  for (s in 1:num_sims) {
    # Selects two clusters for mixture of two highly centered components.
    X1 <- rnorm(n/g, 0, 0.5)
    X2 <- rnorm(n/g, 5, 0.5)
    X <- as.matrix(c(X1, X2), nrow = n)
    K <- 5
    ghat <- gclust(X, K)
    if (ghat == g) {
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
    X1 <- rnorm(n/g, 0, 0.5)
    X2 <- rnorm(n/g, 4, 0.5)
    X3 <- rnorm(n/g, 8, 0.5)
    X <- as.matrix(c(X1, X2, X3), nrow = n)
    K <- 5
    ghat <- gclust(X, K)
    if (ghat == g) {
      num_right <- num_right + 1
    } else {
      num_wrong <- num_wrong + 1
    }
  }
  expect_true(num_right > num_wrong)

})

test_that("BIC higher for correct model than misspecified model.", {

  set.seed(123)
  num_sims <- 30

  result <- lapply(1:num_sims, function(i) {
    # Generate 3-Block SBM
    n <- 120
    num_class1 <- n/3
    num_class2 <- n/3

    # Params
    num_class3 <- n - num_class1 - num_class2
    assignments <- c(rep(1, num_class1), rep(2, num_class2), rep(3, num_class3))
    B <- matrix(c(0.8, 0.3, 0.2,
                  0.3, 0.8, 0.3,
                  0.2, 0.3, 0.8), nrow = 3)

    # Simulation.
    g <- igraph::sample_sbm(n,
                            pref.matrix=B,
                            block.sizes=c(num_class1, num_class2, num_class3))

    # Embed.
    X <- igraph::embed_adjacency_matrix(g, 3)$X

    # Model selection.
    Kmax <- 5
    Khat <- gclust(X, Kmax)

    # Cluster. In one we choose BIC-optimal K (likely to be 3).
    # In the other, we force a clustering with K = 2. gclust should select a K
    # with the higher BIC.
    bic_gclust <- mclust::Mclust(X, Khat)$bic
    bic_k2gmm <- mclust::Mclust(X, 2)$bic
    return(list(bic_gclust = bic_gclust, bic_k2gmm = bic_k2gmm))
  })

  # Split results into separate vectors.
  result_bic_gclust <- sapply(result, function(res){res$bic_gclust})
  result_bic_k2gmm <- sapply(result, function(res) {res$bic_k2gmm})

  # Test difference in BIC via Wilcoxon Test.
  alpha <- 0.05
  pval <- wilcox.test(result_bic_gclust, result_bic_k2gmm, alt='greater', exact=FALSE)$p.value
  expect_lt(pval, alpha)

})
