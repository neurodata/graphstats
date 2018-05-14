## ---- warning=FALSE, message=FALSE---------------------------------------
require(graphstats)
require(mclust)
require(MASS)
require(ggplot2)

## ---- fig.height=4, fig.width=5------------------------------------------

# Mixture parameters
mu0 <- -1.5
mu1 <- 2
sigma0 <- 1
sigma1 <- 0.5
pi0 <- 0.5
pi1 <- 1 - pi0

# Defined separate and mixed PDFs as functions.
mixture_pdf <- function(x) { pi0*dnorm(x, mu0, sigma0) + pi1*dnorm(x, mu1, sigma1) }
norm0_pdf <- function(x) { pi0*dnorm(x, mu0, sigma0) }
norm1_pdf <- function(x) { pi1*dnorm(x, mu1, sigma1) }

# Display.
p <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
        stat_function(fun = mixture_pdf, size = 1.5) +
        stat_function(fun = norm0_pdf, colour = "deeppink", size = 1) +
        stat_function(fun = norm1_pdf, colour = "dodgerblue3", size = 1)
p + xlab("X") + ylab("PDF") + ggtitle("Mixture of Two Gaussian Densities")


## ---- fig.height=3.5, fig.width=5----------------------------------------
# Condition on class sizes of pi0*n and pi1*n to sample data matrix X.
set.seed(123)
n <- 100
X0 <- rnorm(pi0*n, mu0, sigma0)
X1 <- rnorm(pi1*n, mu1, sigma1)
X <- as.matrix(c(X0, X1), nrow = n)

# Run gclust, which will consider models from K = 1 to 5.
G <- gclust(X, K = 5)
cat("The BIC-optimal number of components for the data is:", G, "\n")

