#' scatterplot for the cdr3 clonetype correlation between two samples
#'
#' draw scatter plot for the cdr3 clonetype between 2 samples, and add the linear regression formula
#'
#' @param sample1_cdr3 two columns: cdr3aa and frequency for the first sample
#' @param sample2_cdr3 two columns: cdr3aa and frequency for the second sample
#' @param sample1_id
#' @param sample2_id
#' @param file_out the filename of output figue,should end up with pdf or png
#' @return NULL
#' @import ggplot2
#' @import reshape
#' @import gridExtra
#' @import grid
#' @export
#' @keywords cdr3_paired_comparison
#' @examples
#' #' cdr3_paired_comparison(sample1_cdr3,sample2_cdr3,'case','control','cdr3_paired_comparison.pdf')

cdr3_paired_comparison <- function(sample1_cdr3,sample2_cdr3,sample1_id,sample2_id,file_out){
  require(ggplot2); require(grid); require(gridExtra); require(reshape);

  # transform data
  to_double = function(x) {
    log10(as.numeric(as.character(x))+1e-7)
  }

  colnames(sample1_cdr3)=c('cdr3aa','freq')
  colnames(sample2_cdr3)=c('cdr3aa','freq')
  xy <- merge(sample1_cdr3,sample2_cdr3,by='cdr3aa',all=T)
  colnames(xy)=c('cdr3aa','x','y')
  xy$x <- to_double(xy$x)
  xy$y <- to_double(xy$y)

  xx=sample1_cdr3;yy=sample2_cdr3
  xx$xx <- to_double(sample1_cdr3$freq)
  yy$yy <- to_double(sample2_cdr3$freq)

  xmin <- min(xx$xx, yy$yy)

  # For regression info plotting. source: http://goo.gl/K4yh
  lm_eqn = function(df){
    m = lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                     list(a = format(coef(m)[1], digits = 2),
                          b = format(coef(m)[2], digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq))
  }


  # plotting function for simplicity
  my.plot <- function(...) {

    # placeholder (top right)

    empty <- ggplot() +
      geom_point(aes(1,1), colour="white") +
      theme(
        plot.background  = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        axis.title.x     = element_blank(),
        axis.title.y     = element_blank(),
        axis.text.x      = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks       = element_blank(),
        plot.margin      = unit(c(3, -5.5, 4, 3), "mm")
      )

    # scatterplot

    scatter <- ggplot() +
      theme_bw() +
      geom_point(data = xy, aes(x, y, size = (x + y) / 2),
                 fill   = "red",
                 colour = "black",
                 alpha  = 0.4,
                 pch    = 21
      ) +
      geom_text(data = data.frame(), aes(x = xmin, y = 0, label = lm_eqn(xy)), hjust = 0, parse = TRUE) +
      stat_smooth(data = xy, aes(x, y, weight = 10^((x + y) / 2)), color= "gray25", method = "lm", fullrange = T) +
      scale_x_continuous(limit = c(xmin, 0), expand = c(0, 0.1)) +
      scale_y_continuous(limit = c(xmin, 0), expand = c(0, 0.1)) +
      scale_size_continuous(guide = "none", range = c(1, 10)) +
      xlab(sample1_id) +
      ylab(sample2_id) +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1))

    # marginal density of x

    plot_top <- ggplot() +
      stat_density(data=xx, aes(x=xx, weight=10^xx/sum(10^xx), y = ..scaled..),
                   fill = "grey50", colour = "gray25", size = 0.1, alpha = 0.4, adjust = 1) +
      stat_density(data=xy, aes(x=x, weight=10^x/sum(10^x), y = ..scaled..),
                   fill = "red", colour = "gray25", size = 0.1, alpha = 0.4, adjust = 1) +
      scale_x_continuous(limit = c(xmin, 0), expand = c(0, 0.25)) +
      ylab("") + theme_bw() +
      theme(legend.position = "none", axis.title.x = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank(),
            panel.grid  = element_blank())


    # marginal density of y

    plot_right <- ggplot() +
      stat_density(data=yy, aes(x=yy, weight=10^yy/sum(10^yy), y = ..scaled..),
                   fill = "grey50", colour = "gray25", size = 0.1, alpha = 0.4, adjust = 1) +
      stat_density(data=xy, aes(x=y, weight=10^y/sum(10^y), y = ..scaled..),
                   fill = "red", colour = "gray25", size = 0.1, alpha = 0.4, adjust = 1) +
      scale_x_continuous(limit = c(xmin, 0), expand = c(0, 0.2)) +
      coord_flip() +
      ylab("") + theme_bw() +
      theme(legend.position = "none", axis.title.y = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank(),
            panel.grid  = element_blank())

    # arrange the plots together, with appropriate height and width for each row and column

    grid.arrange(plot_top, empty, scatter, plot_right, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

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

  custom.dev(file_out)

  my.plot()

  dev.off()


}
