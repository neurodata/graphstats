## ---- warning=FALSE, message=FALSE---------------------------------------
require(igraph)
require(graphstats)
require(mclust)
require(ggplot2)
require(gridExtra)

## ---- fig.height=3.5, fig.width=5----------------------------------------
# Generate Adjacency Matrix
sbm <- function(epsilon) {
  n <- 100
  blocks <- c(rep(0, n*0.4), rep(1, n*0.6))
  A <- matrix(rep(0, n*n), nrow = n)
  for (i in 2:n) {
    for (j in 1:(i-1)) {
      if (blocks[i] == blocks[j]) {
        A[i, j] <- rbinom(1, 1, 0.23 + epsilon)
      } else {
        A[i, j] <- rbinom(1, 1, 0.76)
      }
      A[j, i] <- A[i, j]
    }
  }
  g <- igraph::graph_from_adjacency_matrix(A)
  g
}

## ---- fig.height=3.5, fig.width=5----------------------------------------
g = sbm(0)
g_epsilon = sbm(0.01)

## ---- fig.height=3, fig.width=8------------------------------------------
A_sbm <- igraph::as_adj(g)
A = matrix(A_sbm, nrow = 100)
A_epsilon_sbm <- igraph::as_adj(g_epsilon)
A_epsilon = matrix(A_epsilon_sbm, nrow = 100)
g1 = gs.plot.plot_matrix(A, legend.name = "connection", xlabel = "vertex", 
                    ylabel = "vertex", title = "Graph Simulated from SBM")
g2 = gs.plot.plot_matrix(A_epsilon, legend.name = "connection", 
                         xlabel ="vertex", ylabel = "vertex")
grid.arrange(g1, g2, nrow = 1)

## ---- fig.height=3.5, fig.width=5----------------------------------------
embed.graph <- function(g, dim) {
  # Call ase to get latent position
  lpv = graphstats::ase(g, dim)
  for (i in 1:dim) {
    if (sign(lpv[1, i]) != 1) {
      lpv[, i] = -lpv[, i]
    }
  }
  return(lpv)
}
Xhat = embed.graph(g, 2)
Xhat_epsilon = embed.graph(g_epsilon, 2)
Xhat_df = as.data.frame(Xhat)
Xhat_epsilon_df = as.data.frame(Xhat_epsilon)
gg <- ggplot(Xhat_df, aes(x=V1, y=V2, color = "Graph 1")) + 
  geom_point(size=1, shape=1)
gg + geom_point(data = Xhat_epsilon_df, aes(x=V1, y=V2,color = "Graph 2"),
                size=1, shape=1) + 
  labs(title="Latent Positions of Two Graphs", x="X", y="Y") + 
  theme(plot.title = element_text(hjust = 0.5))

## ---- fig.height=4, fig.width=6------------------------------------------
np = nonpar(g, g_epsilon)
np$plot

