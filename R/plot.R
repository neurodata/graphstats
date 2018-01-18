#' Graph plot
#'
#' A function that plots a graph.
#'
#' @import ggplot2
#' @import reshape2
#' @import plyr
#' @param mtx [n, n] or [n, d]: the input. Can either be a square matrix or a data matrix.
#' @param title="" the title for the square plot.
#' @param xlabel=""	the x label for the square plot.
#' @param ylabel="" the y label for the square plot.
#' @param legend.name="" the legend title for the square plot.
#' @param legend.show=TRUE whether to show the legend on the plot.
#' @param font.size=12 the default font size for the plot text. Axis/legend text is font.size - 2.
#' @param limits=NULL if the limits are not specified, limits are c(min(mtx), max(mtx)) for color fill.
#' @param vfactor=FALSE if TRUE, interpret the vertex labels as a factor.
#' @param ffactor=FALSE if TRUE, interpret the fill colors as a factor.
#' @return plot the matrix as a plot.
#' @author Eric Bridgeford
#' @export
gs.plot.plot_matrix <- function(mtx, title="",xlabel="", ylabel="", legend.name="metric", legend.show=TRUE,
                                font.size=12, limits=NULL, vfactor=FALSE, ffactor=FALSE, vlist=NULL) {
  dm <- reshape2::melt(mtx)
  if (is.null(limits)) {
    limits <- c(min(mtx), max(mtx))
  }
  colnames(dm) <- c("x", "y", "value")
  if (vfactor) {
    dm$x <- factor(dm$x)
    dm$y <- factor(dm$y)
    if  (!is.null(vlist)) {
      dm$x <- plyr::mapvalues(dm$x, from=1:length(vlist), to=vlist)
      dm$y <- plyr::mapvalues(dm$y, from=1:length(vlist), to=vlist)
      dm$x <- ordered(dm$x, levels=vlist)
      dm$y <- ordered(dm$y, levels=vlist)
    }
  }
  if (ffactor) {
    dm$value <- factor(dm$value)
  }
  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
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
