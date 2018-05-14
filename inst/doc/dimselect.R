## ---- warning=FALSE, message=FALSE---------------------------------------
require(graphstats)
require(ggplot2)
require(igraph)
require(reshape2)

## ---- fig.height=3.5, fig.width=5----------------------------------------
# Assumes distribution of eigenvalues follow uniform distribution
d1=runif(10,0,10) # First 10 eigenvalues
d2=runif(10,15,25) # Last 10 eigenvalues
d = c(d1,d2)
d = sort(d,decreasing = TRUE)
df <- data.frame(1:20, d)
colnames(df) <- c("Rank", "Eigenvalue")
g <- ggplot(df, aes(Rank, Eigenvalue))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
      labs(title="Screeplot (Distribution of Eigenvalues)") +
      scale_x_continuous("Rank", breaks = seq(1, 20, 1)) + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))

## ---- fig.height=3.5, fig.width=5----------------------------------------
set.seed(123)
# SBM Params
n <- 100
num_class1 <- n/2
num_class2 <- n - num_class1
assignments <- c(rep(1, num_class1), rep(2, num_class2))

B_sbm <- matrix(c(0.8, 0.2,
                  0.2, 0.8), nrow = 2)

# 2-block simulation.
g_sbm <- igraph::sample_sbm(n, pref.matrix=B_sbm, block.sizes=c(num_class1, num_class2))

## Embed both with ASE; get singular values from adjacency matrix;
## select dimenstion with dimselect.
A_sbm <- igraph::as_adj(g_sbm)
A = matrix(A_sbm, nrow = 100)
gs.plot.plot_matrix(A, legend.name = "connection", xlabel = "vertex", 
                    ylabel = "vertex", title = "Graph Simulated from SBM")

## ---- fig.height=3.5, fig.width=5----------------------------------------
sigma_sbm <- svd(A, n)$d
dim_sbm <- dimselect(sigma_sbm)[1]
sprintf("The number of dimension selected by dimselect.R: %d", dim_sbm)

## ---- fig.height=4, fig.width=6------------------------------------------
df <- data.frame(1:n, sigma_sbm)
colnames(df) <- c("Rank", "Eigenvalue")
g <- ggplot(df[1:20,], aes(Rank, Eigenvalue))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
      labs(title="Screeplot (Distribution of Singular Values)") +
      scale_x_continuous("Rank", breaks = seq(1, 20, 1)) + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))

