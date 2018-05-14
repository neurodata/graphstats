## ---- warning=FALSE, message=FALSE---------------------------------------
require(graphstats)
require(mclust)
require(ggplot2)

## ---- fig.height=4.25, fig.width=5---------------------------------------

# SBM parameters.
set.seed(123)
n <- 50
B <- matrix(c(0.75, 0.25,
              0.25, 0.75), nrow = 2)
block_sizes <- c(n/2, n/2)
true_block_assignments <- rep(c(1, 2), block_sizes)

# Sample and save adjacency matrix.
g <- igraph::sample_sbm(n, B, block_sizes)
A <- igraph::as_adj(g, sparse = FALSE)

# Display.
gs.plot.plot_matrix(A, title="High Within-Block Edge Density", legend.name="Connection", ffactor = TRUE)


## ---- fig.height=4.25, fig.width=5---------------------------------------

# Spectral Graph Clustering on G.
SGC1 <- sgc(g)


## ---- fig.height=4.25, fig.width=5---------------------------------------

# Comparison of cluster assignments.
predicted_block_assignments <- SGC1$Y

# Adjusted Random Index 
ari <- mclust::adjustedRandIndex(predicted_block_assignments, true_block_assignments)
cat("The ARI of the true and predicted block labels is:", ari, "\n")


## ---- fig.height=4.25, fig.width=5---------------------------------------

# SBM parameters.
set.seed(789)
n <- 50
B <- matrix(c(0.25, 0.75,
              0.75, 0.25), nrow = 2)
block_sizes <- c(n/2, n/2)
true_block_assignments <- rep(c(1, 2), block_sizes)

# Sample and save adjacency matrix.
g <- igraph::sample_sbm(n, B, block_sizes)
A <- igraph::as_adj(g, sparse = FALSE)

# Display.
gs.plot.plot_matrix(A, title="High Between-Block Edge Density", legend.name="Connection", ffactor = TRUE)


## ---- fig.height=4.25, fig.width=5---------------------------------------

# Spectral Graph Clustering on G.
SGC2 <- sgc(g)

# Comparison of cluster assignments.
predicted_block_assignments <- SGC2$Y

# Adjusted Random Index 
ari <- mclust::adjustedRandIndex(predicted_block_assignments, true_block_assignments)
cat("The ARI of the true and predicted block labels is:", ari, "\n")


## ---- fig.height=4.25, fig.width=5---------------------------------------

# Retrieve latent vector estimates.
X1 <- SGC1$ase$X
X2 <- SGC2$ase$X

# Display.

dat1 <- data.frame(X1)
colnames(dat1) <- c("x", "y")
dat1$color <- factor(true_block_assignments)
p1 <- ggplot(dat1, aes(x = x, y = y, color=color)) + geom_point()
p1 + xlab("Dimension 1") + ylab("Dimension 2") +  
  scale_color_discrete(name="True Block") +
  ggtitle("ASE of Graph 1: High Within-Block Edge Density") + 
  xlim(-1, 1) + ylim(-0.8, 0.8)

## ---- fig.height=4.25, fig.width=5---------------------------------------
dat2 <- data.frame(X2)
colnames(dat2) <- c("x", "y")
dat2$color <- factor(true_block_assignments)
p2 <- ggplot(dat2, aes(x = x, y = y, color=color)) + geom_point()
p2 + xlab("Dimension 1") + ylab("Dimension 2") + 
  scale_color_discrete(name="True Block") +
  ggtitle("ASE of Graph 2: High Between-Block Edge Density")  + 
  xlim(-1, 1) + ylim(-0.8, 0.8)

