## ---- warning=FALSE, message=FALSE---------------------------------------
require(graphstats)
require(mclust)
require(ggplot2)

## ---- fig.height=4.25, fig.width=5---------------------------------------

# Create latent vectors, and edge probability matrix.
block1 <- as.matrix(c(0.85, 0), nrow = 2)
block2 <- as.matrix(c(0.3, 0.8), nrow = 2)
block_probs <- matrix(c(t(block1) %*% block1,
                        t(block1) %*% block2,
                        t(block2) %*% block1,
                        t(block2) %*% block2), ncol = 2)

# Create SBM. Use higher edge probability if nodes are from same block.
set.seed(456)
n <- 60
block.sizes <- c(n/2, n/2)
blocks <- rep(c(1, 2), block.sizes)
g <- igraph::sample_sbm(n, block_probs, block.sizes)
gs.plot.plot_matrix(igraph::as_adjacency_matrix(g, sparse = FALSE),
                    legend.name="Connection",
                    ffactor = TRUE)

## ---- fig.height=4, fig.width=4------------------------------------------
# Embed graph into R^2.
dim <- 2
X <- ase(g, dim)
dat <- as.data.frame(X)

# Display.
p <- ggplot(dat) + geom_point(aes(x = V1, y = V2), color=blocks)
p + xlab("PC Score 1") + ylab("PC Score 2")

## ---- fig.height=4, fig.width=4------------------------------------------
# Cluster using EM algorithm.
model <- Mclust(X, verbose = FALSE)
predictions <- round(model$z[,2])

# Find cluster means. Rotate the data to be around latent vectors via Procrustes.
means <- model$parameters$mean
latent_vecs <- cbind(block1, block2)
M <- svd(means %*% t(latent_vecs))
R <- M$u %*% t(M$v)
X_R <- X %*% R

# Display.
dat <- as.data.frame(X_R)
vec <- as.data.frame(t(latent_vecs))
p <- ggplot(dat) + geom_point(aes(x = V1, y = V2), color=blocks) +
  geom_point(data = vec, mapping = aes(x=V1, y=V2, shape=4), size = 5) + 
  scale_shape_identity()

p + xlab("PC Score 1") + ylab("PC Score 2")

