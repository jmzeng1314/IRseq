#' Heatmap for V/D/J usage among samples
#'
#' It will generate a published quality figure according to the V/D/J usage matrix
#'
#' @param usage_matrix cols are V/D/J, rows are samples
#' @param file_out the filename of output figue,should end up with pdf or png
#' @return NULL
#' @import gplots
#' @import ggplot2
#' @export
#' @keywords heatmap
#' @examples
#' #' plot_vdj_usage_ht(v_usage_matrix,'v-usage-heatmap.pdf')

plot_vdj_usage_ht <- function(usage_matrix,file_out){
  require(gplots); require(RColorBrewer); require(ggplot2); require(plotrix)
  x=usage_matrix
  # plotting function for simplicity
  my.plot <- function(...) {
    heatmap.2(t(x),  na.rm=F, labCol = rownames(x),
              na.col="grey50",
              col=colorRampPalette(c("#2f98ce", "#e0f3db", "#f47104")),
              density.info="none", trace="none",scale="column",
              ...)
  }

  custom.dev <- function(fname) {
    if (grepl("\\.pdf$",fname)){
      pdf(fname)
    } else if (grepl("\\.png$",fname)) {
      png(fname, width     = 3.25,
          height    = 3.25,
          units     = "in",
          res       = 1200,
          pointsize = 4)
    } else {
      stop('Unknown plotting format')
    }
  }

  # layout plot
  fig <- c(0.05, 0.95, 0, 1.0)
  mar <- c(0, 0, 0, 0)

  layout(matrix(1:2, ncol=2), width = c(2, 0.1), height = c(1, 1))

  par(fig = fig, mar = mar, xpd = NA) # this ensures labels are not cut
  if(hasArg(file_out)){
    custom.dev(file_out)
    my.plot()
    dev.off()
  }else{
    my.plot()
  }

}
