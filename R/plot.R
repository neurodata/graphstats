#' Graph plot
#'
#' A function that plots an igraph object, as an adjacency matrix.
#'
#' @import ggplot2
#' @import igraph
#' @importFrom reshape2 melt
#' @param g input graph, as an igraph object. See \code{\link[igraph]{graph}} for details.
#' @param title the title for the square plot. Defaults to \code{""}.
#' @param src.label the source label for the graph. Defaults to \code{"Vertex"}.
#' @param tgt.label the target label for the graph. Defaults to \code{"Vertex"}.
#' @param edge.attr the name of the attribute to use for weights. Defaults to \code{NULL}.
#' \itemize{
#' \item{\code{is.null(edge.attr)} plots the graph as a binary adjacency matrix.}
#' \item{\code{is.character(edge.attr)} plot the graph as a weighted adjacency matrix, with edge-weights for \code{E(g)} given by \code{E(g)[[edge.attr]]}.}
#' }
#' @param font.size the default font size for the plot text. Axis/legend text is \code{font.size - 2}. Defaults to \code{NULL}.
#' \itemize{
#' \item{\code{is.null(font.size)} uses the default sizing for all fonts.}
#' \item{\code{!is.null(font.size)} uses \code{font.size} as the font sizing for the plots.}
#' }
#' @param vertex.label an attribute for naming the vertices. Defaults to \code{NULL}.
#' \itemize{
#' \item{\code{vertex.label==FALSE} name the vertices \code{V(g)} sequentially, as 1, 2, ... n.}
#' \item{\code{vertex.label==TRUE} name the vertices \code{V(g)} as \code{V(g)$name}.}
#' }
#' @param vertex.attr an attribute to color vertices. Defaults to \code{FALSE}.
#' \itemize{
#' \item{\code{vertex.attr==FALSE} assumes no grouping of the vertices, and adds no color accordingly.}
#' \item{\code{is.character(vertex.attr)} assumes a grouping of the vertices given for \code{V(g)} by \code{V(G)[[vertex.attr]]}, and groups the vertices in \code{V(g)} into ordered blocks with color-coding.}
#' }
#' @param edge.xfm log-transform the edge-weights. Defaults to \code{FALSE}.
#' \itemize{
#' \item{\code{edge.xfm==FALSE} do not transform the edge-weights.}
#' \item{\code{edge.xfm == "log"} transform the edge values, using the natural-logarithm operation. See \code{\link[base]{log}} for details. Does not work if there are negative edge-weights.}
#' \item{\code{edge.xfm == "log10"} transform the edge values, using the logarithm-base-10 operation. See \code{\link[base]{log10}} for details. Does not work if there are negative edge-weights.}
#' \item{\code{! (edge.xfm %in% c(FALSE, "log", "log10"))} assumes \code{edge.xfm} is a function, and transform the vertices with the provided function.}
#' }
#' @return the matrix as a plot.
#' @author Eric Bridgeford
#' @export
gs.plot.plot_adjacency <- function(g, title="",src.label="Vertex", tgt.label="Vertex", edge.attr=NULL,
                                font.size=NULL, vertex.label=FALSE, vertex.attr=FALSE, edge.xfm=FALSE, eps=0.0001) {
  # load adjacency matrix as a dense matrix
  adj <- as_adjacency_matrix(g, attr=edge.attr, names=vertex.label, type="both", sparse=FALSE)
  adj.data <- melt(adj)  # melt the graph to a data-frame with row and colnames preserved
  colnames(adj.data) <- c("Source", "Target", "Weight")
  if (!is.character(edge.attr)) {
    adj.data$Weight <- factor(adj.data$Weight, levels=c(0, 1), ordered=TRUE)
    edge.colors <- c("#ffffff", "#000000")  # binarize the graph as white or black according to 0 or 1
    names(edge.colors) <- levels(adj.data$Weight)  # name the connections 0 or 1 accordingly
    wt.name <- "Connection"
  } else {
    wt.name <- edge.attr
    if (edge.xfm != FALSE) {
      if (edge.xfm == "log") {
        edge.xfm=log
        # set 0-weight edges to far lower than rest of graph
        adj.data$Weight <- adj.data$Weight + eps
        wt.name = sprintf("log(%s)", wt.name)
      } else if (edge.xfm == "log10") {
        edge.xfm = log10
        # set 0-weight edges to far lower than rest of graph
        adj.data$Weight <- adj.data$Weight + eps
        wt.name = sprintf("log10(%s)", wt.name)
      }
      adj.data$Weight <- do.call(edge.xfm, list(adj.data$Weight))
    }
    # color the vertices on a purple scale
    edge.colors <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3",  "#4a1486")
  }
  #if (is.character(vertex.attr)) {
    # reorder the vertices so that vertices in same group are together

  #}
  plot.adj <- ggplot(adj.data, aes(x=Source, y=Target, fill=Weight)) +
    geom_tile() +
    xlab(src.label) +
    ylab(tgt.label) +
    ggtitle(title) +
    theme_bw()
  if (vertex.label) {
    plot.adj <- plot.adj + theme(axis.text.x = element_text(angle=60, hjust=1))
  }
  if (is.character(edge.attr)) {
    plot.adj <- plot.adj +
      scale_fill_gradientn(colors=edge.colors, name=wt.name)
  } else {
    plot.adj <- plot.adj +
      scale_fill_manual(values=edge.colors, name=wt.name)
  }
  #if (is.character(vertex.attr)) {
  #  vertices <- colnames(adj)
  #  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  #  V.gr <- factor(get.vertex.attribute(g, vertex.attr))  # group the vertices by their attribute
  #  un.vertices <- levels(V.gr)
  #  attr.colors <- jet.colors(length(un.vertices))
  #  for (i in 1:length(attr.colors)) {
  #    vertex.num <- min(which())
  #    plot.adj <- plot.adj +
  #      geom_rect()
  #  }
  #  plot.adj <- plot.adj +
  #    geom_tile(data=adj.data, aes(x=Source, y=Target, fill=Fill1), show.legend=TRUE, alpha=0.2) +
  #    geom_tile(data=adj.data, aes(x=Source, y=Target, fill=Fill2), show.legend=FALSE, alpha=0.2)
  #}
  return(plot.adj)
}
