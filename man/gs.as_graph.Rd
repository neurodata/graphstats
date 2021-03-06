% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{gs.as_graph}
\alias{gs.as_graph}
\title{To Graph Helper}
\usage{
gs.as_graph(A, gref = NULL, edge.attr = NULL)
}
\arguments{
\item{A}{the n x n adjacency matrix.}

\item{gref}{a reference graph to use for adding vertex attributes.}

\item{edge.attr}{the edge attribute to (optionally) add weights from the adjacency matrix to.}
}
\value{
the graph as an igraph object.
}
\description{
reconstruct a matrix from an adjacency matrix using vertex attributes retained from
a reference graph that was passed as input.
}
\author{
Eric Bridgeford
}
\keyword{internal}
