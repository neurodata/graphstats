## ---- warning=FALSE, message=FALSE---------------------------------------
suppressMessages(require(graphstats))
suppressMessages(require(igraph))
suppressMessages(require(ggplot2))

## ---- fig.width = 5, fig.height=4----------------------------------------

# Number of seeds, total nubmer of vertices, and edge probability.
m <- 3
n <- 10
p <- 0.5
set.seed(123)

# Sample graph 1, and permute to graph 2; delete a vertex to show VN works
# when total number of vertices are different.
g1 = sample_sbm(n, as.matrix(p), n)
permute = c(1:m, sample(n-m)+m)
g2 = permute.vertices(g1, permute)
g2 = delete.vertices(g2, 10)

seeds <- matrix(cbind(1:m, 1:m), nrow = m)
x = 4
h = 1
ell = 2
R = 100
gamma = 0.01
vn = vnsgm(x,seeds,g1,g2,h,ell,R,gamma,plotF = TRUE)

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
g1 = sampled_graphs[[1]]
g2 = sampled_graphs[[2]]
A <- as_adj(g1, sparse = FALSE)
B <- as_adj(g2, sparse = FALSE)
# Display overlap.
gs.plot.plot_matrix(A + B, title="A + B (Overlap)", legend.name="A_ij + B_ij")

## ---- fig.width=4.5, fig.height=4----------------------------------------

# Identify seeds.
m <- 2
seeds <- matrix(cbind(1:m, 1:m), nrow = m)

# Set up permutation.
permute = c(1:m, sample(n-m)+m)
# Apply permutation and delete 1 vertex from graph 2 two make vertex set size different for two graphs.
g2_p = permute.vertices(g2, permute)
g2_p = delete.vertices(g2_p, 10)
vn = vnsgm(x,seeds,g1,g2_p,h,ell,R,gamma,plotF = TRUE)

## ---- fig.width=5, fig.height=4------------------------------------------
nominated.vertex = as.numeric(colnames(vn$P)[apply(vn$P,1,which.max)])
df = as.data.frame(rbind(permute,nominated.vertex))
row.names(df) = c('True Permutation of G1','VN Permutation')
df

