#' Run spectral clustering on a (possibly directed) (possibly weighted) graph.
#'
#' It does
#' 1. do a pass-to-rank for a weighted graph (\eqn{PTR}, no-op for an unweighted graph),
#' 2. do a graph spectral embedding (\code{ASE} or \code{LSE}) with a _diagonal augmentation_,
#' 3. do a _dimension reduction_ (\eqn{ZG}) and merge left and right vectors (no-op for an undirected graph),
#' 4. cluster vertices (\eqn{GMM} or \eqn{Kmeans}).
#'
#' @param g a graph in \code{igraph} format
#' @param dmax maximum dimension for embedding
#' @param Kmax maximum number of clusters
#' @param elb an index for elbow
#' @param abs a boolean to take abs on elbow finder or not
#' @param embed either \code{ASE} or \code{LSE}, spectral embedding method
#' @param clustering either \code{GMM} or \code{Kmeans}, clustering method
#' @param weight either \code{ptr} pr \code{binary} or \code{raw} to determine whether to perform pass-to-rank or not, default is \code{raw}
#' @param verbose boolean to determine whether to display an intermediate fitting progress status of \code{mclust} or not, default is \code{TRUE}
#' @param doplot boolean to determine whether to draw plots or not, default is \code{TRUE}
#'
#' @return \code{g} the largest connected component of the input graph
#' @return \code{ase} ASE or LSE output object
#' @return \code{elb} number of dimensions to which the graph was reduced.
#' @return \code{mc} clustering output object
#' @return \code{Y} labels for the clustering
#' @references D.L. Sussman, M. Tang, D.E. Fishkind, and C.E. Priebe,
#' A consistent adjacency spectral embedding for stochastic blockmodel graphs,
#' Journal of the American Statistical Association, Vol. 107, No. 499, pp. 1119-1128, 2012.
#'
#' @examples
#' library(igraph)
#' data(g)
#' E(g)$weight <- runif(ecount(g), 1, 5) # add random edge weights
#' Y <- gmmase(g, dmax=20, use.ptr=TRUE, embed="ASE", clustering="Kmeans")
#'
#' @author Youngser Park <youngser@jhu.edu>
#' @export
sgc <- function(g,
                dmax=2,
                elb=1,
                abs=FALSE,
                lcc=TRUE,
                embed="ASE",
                clustering="GMM",
                Kmax=9,
                weight="raw",
                verbose=TRUE,
                doplot=FALSE)
{

  # Validate Input Arguments.
  validateInput.sgc(g,dmax,elb,abs,lcc,embed,clustering,Kmax,weight,verbose,doplot)

  # If spectral clustering is to be performed on only the largest conncected component,
  # let that be the graph of interest.
  if (lcc) {
    cat("1. Finding an lcc...\n")
    # finding the largest connected component
    cl <- igraph::clusters(g)
    g <- igraph::induced.subgraph(g, which(cl$membership == which.max(cl$csize)))
  }
  summary(g)

  # Select weight representation of graph.
  if (igraph::is.weighted(g)) {
    if (weight=="ptr") {
      cat("2. Passing-to-rank...\n")
      g <- rank(g)
    } else if (weight=="binary") {
      cat("2. Binarizing the edge weight...\n")
      E(g)$weight <- ifelse(E(g)$weight > 0, 1, 0)
    } else {
      cat("2. Using the raw edge weight...\n")
    }
  }

  # Embed the graph into latent space via ASE or LSE.
  cat(paste0("3. Embedding the graph into dmax = ", dmax, "...\n"))
  if (embed=="ASE") {
    ase <- igraph::embed_adjacency_matrix(g,dmax,options=list(maxiter=10000))
  } else {
    #        ase <- embed_laplacian_matrix(g,dmax,options=list(maxiter=10000))
    ase <- igraph::embed_laplacian_matrix(g,dmax,type="DAD",options=list(maxiter=1000))
  }

  # Reduce dimensionality.
  cat("4. Finding an elbow (dimension reduction)...")
  if (abs) D <- abs(ase$D) else D <- ase$D
  elb <- max(dimselect(D, n=elb, plot=doplot)[elb], 2)
  cat(", use dhat = ", elb,"\n")
  Xhat1 <- ase$X[,1:elb]
  if (!igraph::is.directed(g)) Xhat2 <- NULL else Xhat2 <- ase$Y[,1:elb]
  Xhat <- cbind(Xhat1,Xhat2)

  # Cluster vertices in latent space.
  cat("5. Clustering vertices...")
  mc <- Y <- NULL
  if (clustering %in% c("GMM","gmm")) {
    if (length(Kmax)>1) {
      mc <- mclust::Mclust(Xhat, Kmax, verbose=verbose)
    } else {
      mc <- mclust::Mclust(Xhat,2:Kmax, verbose=verbose)
    }
    cat(", Khat = ", mc$G, "\n")
    if (doplot) plot(mc,what="BIC")
    print(summary(mc))
    Y <- mc$class
  } else if (clustering %in% c("DA", "da")) {

  } else {
    usepam <- ifelse(igraph::vcount(g)>2000, FALSE, TRUE)
    crit <- ifelse(igraph::vcount(g)>2000, "multiasw", "asw")
    if (length(Kmax)>1) {
      mc <- fpc::pamk(Xhat, Kmax, usepam=usepam, criterion=crit)
    } else {
      mc <- fpc::pamk(Xhat,2:Kmax,usepam=usepam, criterion=crit)
    }
    if (doplot) plot(mc$crit, type="b")
    Y <- mc$pamobj$cluster
    cat(", Khat = ", max(Y), "\n")
    print(table(Y))
    mc$data <- Xhat
    mc$class <- Y
  }

  return(list(g=g,ase=ase,elb=elb,mc=mc,Y=Y))
}

validateInput.sgc <- function(g,dmax,elb,abs,lcc,embed,clustering,Kmax,weight,verbose,doplot)
{
  # Graph
  if (class(g) != 'igraph') { stop("Error: Input object 'g' must be an igraph object.") }

  # Dmax
  if (!is.numeric(dmax) || dmax%%1 != 0 || dmax < 1) {
    stop("Error: Input 'dmax' must be an integer and >=1.")
  }

  # Elbow
  if (!is.numeric(elb) || elb%%1 != 0 || elb < 1) {
    stop("Error: Input 'elb' must be an integer and >=1.")
  }

  # More Elbows than Dimensions
  if (elb > dmax) {
    stop("Error: Number of elbows 'elb' is greater than maximum dimension 'dmax'.")
  }

  # Absolute value, then get elbow.
  if (!is.logical(abs)) { stop("Error: Input 'abs' must be a logical.")}

  # Largest Connected Component
  if (!is.logical(lcc)) { stop("Error: Input 'lcc' must be a logical.")}

  # Embedding
  if (embed != "ASE" && embed != "LSE") {
    stop("Error: 'embed' must be a string equal to 'ASE' or 'LSE'.")
  }

  # Clustering
  if (clustering != "GMM" && clustering != "Kmeans") {
    stop("Error: 'clustering' must be a string equal to 'GMM' or 'Kmeans'.")
  }

  # Kmax
  if (!is.numeric(Kmax) || Kmax%%1 != 0 || Kmax < 1) {
    stop("Error: Input 'Kmax' must be an integer and >=1.")
  }

  # Weight
  if (weight != "ptr" && weight != "binary" && weight != "raw") {
    stop("Error: 'weight' must be a string equal to 'ptr', 'binary', or 'raw'.")
  }

  # Verbose
  if (!is.logical(verbose)) { stop("Error: Input 'verbose' must be a logical.")}

  # Plotting
  if (!is.logical(doplot)) { stop("Error: Input 'doplot' must be a logical.")}

}

