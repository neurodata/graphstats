## ---- warning=FALSE, message=FALSE---------------------------------------
require(graphstats)
require(igraph)
require(ggplot2)

## ---- fig.width=4.5, fig.height=4----------------------------------------

# Number of seeds, total nubmer of vertices, and edge probability.
m <- 3
n <- 10
p <- 0.5
set.seed(12345)

# Sample matrix A, and permute to .
A <- matrix(as_adj(sample_sbm(n, p, n)), nrow = n)
B <- A[c(1:m, sample(n-m)+m),]

# P is the permutation matrix that will turn match B to A.

# Unordered call.
seeds <- matrix(cbind(1:m, 1:m), nrow = m)
P <- sgm(A, B, seeds)

# Ordered call.
start <- diag(n-m)[sample(n-m),]
P <- sgm.ordered(A, B, m, start)

# Display.
gs.plot.plot_matrix(P, title="Permutation Matrix", legend.name="Entries", ffactor = TRUE)

## ---- fig.width=4.5, fig.height=4----------------------------------------

# Apply permutation matrix.
B_matched <- P %*% B %*% t(P)

# Visualize matched graphs.
gs.plot.plot_matrix(A, title="Original A Matrix", legend.name="Entries", ffactor = TRUE)
gs.plot.plot_matrix(B_matched, title="Matched B Matrix", legend.name="Entries", ffactor = TRUE)
gs.plot.plot_matrix(B, title="Original B Matrix", legend.name="Entries", ffactor = TRUE)

## ---- fig.width=5, fig.height=4------------------------------------------
gs.plot.plot_matrix(abs(A-B_matched), title="Disagreements", legend.name="Disagreement", ffactor = TRUE)

## ---- fig.width=4.5, fig.height=4----------------------------------------

# Define latent vectors for 2-SBM RDPG in X.
n <- 10
X1 <- matrix(rep(c(0.85, 0), n/2), nrow = n/2, byrow = TRUE)
X2 <- matrix(rep(c(0.3,0.8), n/2), nrow = n/2, byrow = TRUE)
X <- rbind(X1, X2)
set.seed(6789)

# Pearson correlation coefficient.
r <- 0.75

# Sample r-SBM.
sampled_graphs <- rdpg.sample.correlated(X, r)
A <- as_adj(sampled_graphs[[1]], sparse = FALSE)
B <- as_adj(sampled_graphs[[2]], sparse = FALSE)

# Display overlap.
gs.plot.plot_matrix(A + B, title="A + B (Overlap)", legend.name="A_ij + B_ij")


## ---- fig.width=4.5, fig.height=4----------------------------------------

# Identify seeds.
m <- 2
seed_indices <- c(1:(m/2), 1:(m/2)+n/2)
seeds <- matrix(c(seed_indices, seed_indices), nrow = m)

# Set up permutation.
block1_permutation <- sample(n/2 - m/2) + m/2
block2_permutation <- sample(n/2 - m/2) + m/2 + n/2
B_permutation <- c(1:(m/2), block1_permutation, 1:(m/2)+n/2, block2_permutation)

# Redefine B matrix and match using unordered sgm.
B_p <- B[B_permutation,]
P <- sgm(A, B_p, seeds)

# Apply permutation matrix.
B_matched <- P %*% B_p %*% t(P)

# Display.
gs.plot.plot_matrix(A, title="Original A Matrix", legend.name="Entries", ffactor = TRUE)
gs.plot.plot_matrix(B_matched, title="Matched B Matrix", legend.name="Entries", ffactor = TRUE)
gs.plot.plot_matrix(B_p, title="Original B Matrix", legend.name="Entries", ffactor = TRUE)

## ---- fig.width=5, fig.height=4------------------------------------------
gs.plot.plot_matrix(abs(A-B_matched), title="Disagreements", legend.name="Disagreement", ffactor = TRUE)

