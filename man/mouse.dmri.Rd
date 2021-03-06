% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mouse.dmri}
\alias{mouse.dmri}
\title{Mouse dMRI Connectome}
\format{An \code{\link[igraph]{igraph}} object containing the dMRI graph:
\describe{
\item{\code{V(mouse.dmri)$name}}{The id of the parcellation for the vertex.}
\item{\code{V(mouse.dmri)$level1}}{The level 1 hierarchy the vertex is categorized as. \code{M} is midbrain, \code{F} is forebrain, \code{H} is hindbrain, \code{W} is white matter.}
\item{\code{V(mouse.dmri)$hemisphere}}{The hemisphere of the brain the parcel overlaps maximally with. \code{R} is right; \code{L} is left.}
\item{\code{V(mouse.dmri)$region}}{A composite of the \code{level1} and \code{hemisphere} attributes.}
\item{\code{E(mouse.dmri)$weight}}{The number of fibers connecting the particular two vertices.}
}}
\usage{
mouse.dmri
}
\description{
A graph comprised from a diffusion MRI fiber graph of a mouse brain. Data was collected at the Center for In Vivo Microscopy (Duke University; )
}
\references{
Calabrese, E. et al. A Diffusion MRI Tractography Connectome of the Mouse Brain and Comparison with Neuronal Tracer Data.
\emph{Cerebral Cortex}, 2015.
}
\keyword{datasets}
