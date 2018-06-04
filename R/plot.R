#' Graph Heatmap plot
#'
#' A function that plots an igraph object, as a heatmap.
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
#' @return the graph/graphs as a plot.
#' @author Eric Bridgeford
#' @export
gs.plot.heatmap <- function(g, title="",src.label="Vertex", tgt.label="Vertex", edge.attr=NULL,
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

nan.sum <- function(x) {sum(x, na.rm=TRUE)}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
#' Graph Grid plot
#'
#' A function that plots an igraph object, as a grid, with intensity denoted by the size of dots on the grid.
#'
#' @import ggplot2
#' @import igraph
#' @importFrom reshape2 melt
#' @param g input graph, as an igraph object. See \code{\link[igraph]{graph}} for details.
#' @param title the title for the square plot. Defaults to \code{""}.
#' @param src.label the source label for the graph. Defaults to \code{"Vertex"}.
#' @param tgt.label the target label for the graph. Defaults to \code{"Vertex"}.
#' @param edge.attr the name of the attribute to use for weights. Defaults to \code{NULL}. Can be a list of `edge.attr` if you want to overlay different edge-attributes on the same plot. Supports up to 4 edge-attributes at once.
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
#' @param edge.xfm transform the edge-weights. Defaults to \code{FALSE}. Can be a list of `edge.xfm` if you want to overlay different edge-attributes on the same plot in the plot with different transforms for each.
#' \itemize{
#' \item{\code{edge.xfm==FALSE} do not transform the edge-weights.}
#' \item{\code{edge.xfm == "log"} transform the edge values, using the natural-logarithm operation. See \code{\link[base]{log}} for details. Does not work if there are negative edge-weights. Pads with `eps << min(edge-weight)` if there are entries of zero in the graph.}
#' \item{\code{edge.xfm == "log10"} transform the edge values, using the logarithm-base-10 operation. See \code{\link[base]{log10}} for details. Does not work if there are negative edge-weights. Pads with `eps << min(edge-weight)` if there are entries of zero in the graph.}
#' \item{\code{! (edge.xfm %in% c(FALSE, "log", "log10"))} assumes \code{edge.xfm} is a function, and transform the vertices with the provided function.}
#' }
#' @param eps if you specify an `edge.xfm` that is logarithmic, indicate the padding that zero entries should receive. Defaults to `.0001`. `eps` should be `<< min(edge-weight)`.
#' @param degree Whether to plot the marginal vertex degrees. Defaults to `FALSE`.
#' @return the graph/graphs as a plot.
#' @author Eric Bridgeford
#' @export
gs.plot.grid <- function(g, title="",src.label="Vertex", tgt.label="Vertex", edge.attr=NULL,
                         font.size=NULL, vertex.label=FALSE, vertex.attr=FALSE, edge.xfm=FALSE, eps=0.0001,
                         degree=FALSE) {
  if (!is.vector(edge.attr)) {
    edge.attr <- list(edge.attr)
  }
  if (length(edge.xfm) < length(edge.attr)) {
    edge.xfm <- rep(edge.xfm[1], min(length(edge.attr)))
  }
  cvec <- c("#020202", "#f41711", "#94d6c9", "#5f8793")
  cvec <- cvec[1:length(edge.attr)]
  names(cvec) <- edge.attr
  alpha <- 1/length(edge.attr)
  plot.dat <- data.frame()
  hist.dat <- data.frame()
  for (i in 1:length(edge.attr)) {
    attr <- edge.attr[[i]]; xfm <- edge.xfm[i]
    adj <- as_adjacency_matrix(g, attr=attr, names=vertex.label, type="both", sparse=FALSE)
    adj.data <- melt(adj)  # melt the graph to a data-frame with row and colnames preserved
    colnames(adj.data) <- c("Source", "Target", "Weight")
    #hist.src <- apply(adj, c(1), nan.sum)
    #hist.tgt <- apply(adj, c(2), nan.sum)
    if (is.null(attr)) {
       wt.name = "Connection"
    } else {
      wt.name <- attr
    }
    if (xfm != FALSE) {
      if (xfm == "log") {
        xfm=log
        # set 0-weight edges to far lower than rest of graph
        adj.data$Weight <- adj.data$Weight + eps
        wt.name = sprintf("log(%s)", wt.name)
      } else if (edge.xfm == "log10") {
        xfm = log10
        # set 0-weight edges to far lower than rest of graph
        adj.data$Weight <- adj.data$Weight + eps
        wt.name = sprintf("log10(%s)", wt.name)
      }
      adj.data$Weight <- do.call(xfm, list(adj.data$Weight))
    }
    adj.data <- adj.data[adj.data$Weight != 0,]
    if (length(edge.attr) > 1) {
      adj.data$Weight <- (adj.data$Weight - min(adj.data$Weight, na.rm=TRUE))/(max(adj.data$Weight, na.rm=TRUE) - min(adj.data$Weight, na.rm=TRUE))  # normalize on 0-1
    }
    plot.dat <- rbind(plot.dat, cbind(adj.data, Type=wt.name))
    #hist.dat <- rbind(hist.dat, rbind(data.frame(Degree=hist.src, Type=wt.name, direction="Source"),
    #                                  data.frame(Degree=hist.tgt, Type=wt.name, direction="Target")))
  }
  #hist.dat$Degree <- as.vector(hist.dat$Degree)
  plot.adj <- ggplot(plot.dat, aes(x=Source, y=Target, size=Weight, color=Type, group=Type), alpha=alpha) +
    geom_point() +
    xlab(src.label) +
    ylab(tgt.label) +
    ggtitle(title) +
    theme_bw()
  plot.adj <- plot.adj + scale_color_manual(values=cvec)
  if (degree) {
    plot.adj <- ggMarginal(plot.adj, type="histogram", groupColour = TRUE, groupFill=TRUE)
  }
  #if (is.character(vertex.attr)) {
  # reorder the vertices so that vertices in same group are together

  #}
  if (vertex.label) {
    plot.adj <- plot.adj + theme(axis.text.x = element_text(angle=60, hjust=1))
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

#' Graph Grid plot
#'
#' A function that plots an igraph object, as a series of overlapping heatmaps, with intensity denoted by the colorscale of dots on the heatmap.
#'
#' @import ggplot2
#' @import igraph
#' @importFrom reshape2 melt
#' @param g input graph, as an igraph object. See \code{\link[igraph]{graph}} for details.
#' @param title the title for the square plot. Defaults to \code{""}.
#' @param src.label the source label for the graph. Defaults to \code{"Vertex"}.
#' @param tgt.label the target label for the graph. Defaults to \code{"Vertex"}.
#' @param edge.attr the name of the attribute to use for weights. Defaults to \code{NULL}. Can be a list of `edge.attr` if you want to overlay different edge-attributes on the same plot. Supports up to 4 edge-attributes at once.
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
#' @param edge.xfm transform the edge-weights. Defaults to \code{FALSE}. Can be a list of `edge.xfm` if you want to overlay different edge-attributes on the same plot in the plot with different transforms for each.
#' \itemize{
#' \item{\code{edge.xfm==FALSE} do not transform the edge-weights.}
#' \item{\code{edge.xfm == "log"} transform the edge values, using the natural-logarithm operation. See \code{\link[base]{log}} for details. Does not work if there are negative edge-weights. Pads with `eps << min(edge-weight)` if there are entries of zero in the graph.}
#' \item{\code{edge.xfm == "log10"} transform the edge values, using the logarithm-base-10 operation. See \code{\link[base]{log10}} for details. Does not work if there are negative edge-weights. Pads with `eps << min(edge-weight)` if there are entries of zero in the graph.}
#' \item{\code{! (edge.xfm %in% c(FALSE, "log", "log10"))} assumes \code{edge.xfm} is a function, and transform the vertices with the provided function.}
#' }
#' @param eps if you specify an `edge.xfm` that is logarithmic, indicate the padding that zero entries should receive. Defaults to `.0001`. `eps` should be `<< min(edge-weight)`.
#' @param degree Whether to plot the marginal vertex degrees. Defaults to `FALSE`.
#' @return the graph/graphs as a plot.
#' @author Eric Bridgeford
#' @export
gs.plot.heatmap_overlay <- function(g, title="",src.label="Vertex", tgt.label="Vertex", edge.attr=NULL,
                                    font.size=NULL, vertex.label=FALSE, vertex.attr=FALSE, edge.xfm=FALSE, eps=0.0001,
                                    degree=FALSE) {
  if (!is.vector(edge.attr)) {
    edge.attr <- list(edge.attr)
  }
  if (length(edge.xfm) < length(edge.attr)) {
    edge.xfm <- rep(edge.xfm[1], min(length(edge.attr)))
  }
  cvec <- c("#020202", "#f41711", "#94d6c9", "#5f8793")
  cvec <- cvec[1:length(edge.attr)]
  names(cvec) <- edge.attr
  alpha <- 1/(length(edge.attr))
  plot.dat <- data.frame()
  hist.dat <- data.frame()
  for (i in 1:length(edge.attr)) {
    attr <- edge.attr[[i]]; xfm <- edge.xfm[i]
    adj <- as_adjacency_matrix(g, attr=attr, names=vertex.label, type="both", sparse=FALSE)
    adj.data <- melt(adj)  # melt the graph to a data-frame with row and colnames preserved
    colnames(adj.data) <- c("Source", "Target", "Weight")
    hist.src <- apply(adj, c(1), nan.sum)
    hist.tgt <- apply(adj, c(2), nan.sum)
    if (is.null(attr)) {
      wt.name = "Connection"
    } else {
      wt.name <- attr
    }
    if (xfm != FALSE) {
      if (xfm == "log") {
        xfm=log
        # set 0-weight edges to far lower than rest of graph
        adj.data$Weight <- adj.data$Weight + eps
        wt.name = sprintf("log(%s)", wt.name)
      } else if (edge.xfm == "log10") {
        xfm = log10
        # set 0-weight edges to far lower than rest of graph
        adj.data$Weight <- adj.data$Weight + eps
        wt.name = sprintf("log10(%s)", wt.name)
      }
      adj.data$Weight <- do.call(xfm, list(adj.data$Weight))
    }
    adj.data <- adj.data[adj.data$Weight != 0,]
    if (length(edge.attr) > 1) {
      adj.data$Weight <- (adj.data$Weight - min(adj.data$Weight, na.rm=TRUE))/(max(adj.data$Weight, na.rm=TRUE) - min(adj.data$Weight, na.rm=TRUE))  # normalize on 0-1
    }
    plot.dat <- rbind(plot.dat, cbind(adj.data, Type=wt.name))
    hist.dat <- rbind(hist.dat, rbind(data.frame(Vertex=1:dim(adj)[1], Degree=hist.src/max(hist.src), Type=wt.name, direction="Source"),
                                      data.frame(Vertex=1:dim(adj)[1], Degree=hist.tgt/max(hist.tgt), Type=wt.name, direction="Target")))
  }
  hist.dat$Degree <- as.vector(hist.dat$Degree)

  plot.adj <- ggplot(subset(plot.dat, Type == edge.attr[1]), aes(x=Source, y=Target, fill=Type, alpha=Weight, color=Type, group=Type), alpha=alpha) +
    geom_tile() +
    xlab(src.label) +
    ylab(tgt.label) +
    ggtitle(title) +
    theme_bw()

  if (length(edge.attr) > 1) {
    for (i in 2:length(edge.attr)) {
      plot.adj <- plot.adj +
        geom_point(data=subset(plot.dat, Type == edge.attr[i]), aes(x=Source, y=Target, size=Weight, fill=Type, color=Type, group=Type), alpha=1)
    }
  }
  plot.adj <- plot.adj + scale_color_manual(values=cvec) + scale_fill_manual(values=cvec)
  if (degree) {
    thm = list(theme_void(),
               guides(fill=FALSE),
               theme(plot.margin=unit(rep(0,4), "lines"), legend.position=NaN))
    top.plot <- ggplot(subset(hist.dat, direction == "Source"), aes(x=Vertex, y=Degree, fill=Type, color=Type, group=Type)) +
      geom_bar(stat = "identity", position="identity", alpha=alpha) +
      scale_color_manual(values=cvec) +
      scale_fill_manual(values=cvec) +
      thm
    right.plot <-  ggplot(subset(hist.dat, direction == "Target"), aes(x=Vertex, y=Degree, fill=Type, color=Type, group=Type)) +
      geom_bar(stat = "identity", position="identity", alpha=alpha) +
      scale_color_manual(values=cvec) +
      scale_fill_manual(values=cvec) +
      coord_flip() +
      thm
    empty <- ggplot() + geom_blank() + thm
    pleg <- g_legend(plot.adj)
    plot.adj <- plot.adj + theme(legend.position=NaN)
    widths=c(0.6, 0.2, 0.2)
    heights=c(0.2, 0.8)
    plot.adj <- as_ggplot(arrangeGrob(list(top.plot, empty, empty, plot.adj, right.plot, pleg), byrow=TRUE,
                                      ncol=3, nrow=2, widths=widths, heights=heights, main=title))
  }
  #if (is.character(vertex.attr)) {
  # reorder the vertices so that vertices in same group are together

  #}
  if (vertex.label) {
    plot.adj <- plot.adj + theme(axis.text.x = element_text(angle=60, hjust=1))
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
