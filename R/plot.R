#' Graph plot
#'
#' A function that plots a graph.
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom plyr mapvalues
#' @param graph \code{[v, v]} the input graph.
#' @param title the title for the square plot. Defaults to \code{""}.
#' @param xlabel the x label for the square plot. Defaults to \code{""}.
#' @param ylabel the y label for the square plot. Defaults to \code{""}.
#' @param legend.name the legend title for the square plot. Defaults to \code{"metric"}.
#' @param legend.show whether to show the legend on the plot. Defaults to \code{TRUE}.
#' @param font.size the default font size for the plot text. Axis/legend text is \code{font.size - 2}. Defaults to \code{12}.
#' @param limits the limits for fill color. Defaults to \code{c(min(graph), max(graph))}.
#' @param vfactor if TRUE, interpret the vertex labels as a factor. Defaults to \code{FALSE}.
#' @param ffactor if TRUE, interpret the fill colors as a factor. Defaults to \code{FALSE}.
#' @param vlist an optional list for the vertex labels ordered according to the adjacency matrix \code{graph}. Defaults to \code{NULL}.
#' @return plot the matrix as a plot.
#' @author Eric Bridgeford
#' @export
gs.plot.plot_matrix <- function(mtx, title="",xlabel="", ylabel="", legend.name="metric", legend.show=TRUE,
                                font.size=12, limits=c(min(mtx), max(mtx)), vfactor=FALSE, ffactor=FALSE, vlist=NULL) {
  dm <- melt(mtx)
  colnames(dm) <- c("x", "y", "value")
  if (vfactor) {
    dm$x <- factor(dm$x)
    dm$y <- factor(dm$y)
    if  (!is.null(vlist)) {
      dm$x <- mapvalues(dm$x, from=1:length(vlist), to=vlist)
      dm$y <- mapvalues(dm$y, from=1:length(vlist), to=vlist)
      dm$x <- ordered(dm$x, levels=vlist)
      dm$y <- ordered(dm$y, levels=vlist)
    }
  }
  if (ffactor) {
    dm$value <- factor(dm$value)
  }
  jet.colors <- colorRampPalette(c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3",  "#4a1486"))
  sqplot <- ggplot2::ggplot(dm, aes(x=x, y=y, fill=value)) +
    ggplot2::geom_tile() +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel) +
    ggplot2::ggtitle(title)
  if (ffactor) {
    sqplot <- sqplot +
      ggplot2::scale_fill_discrete(name=legend.name)
  } else {
    sqplot <- sqplot +
      ggplot2::scale_fill_gradientn(colours=jet.colors(7), name=legend.name, limits=limits)
  }
  if (legend.show) {
    sqplot <- sqplot +
      ggplot2::theme(text=element_text(size=font.size))
  } else {
    sqplot <- sqplot +
      ggplot2::theme(text=element_text(size=font.size), legend.position="none")
  }
  if (!is.null(vlist)) {
    sqplot <- sqplot +
      ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  return(sqplot)
}
