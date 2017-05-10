#' visualization for 3 kind of statistics for CDR3 aa
#'
#' The outermost layer will show the top 5  CDR3 aa and their frequencies
#' The middle layer will be the Returns Tukey's five number summary (minimum, lower-hinge, median, upper-hinge, maximum)
#' THe innermost layer will be the unique/double/3 plus CDR3 aa
#'
#' @param stat_matrix  three columns are "type","name","value",3 rows +5 rows + 5 rows
#' @param file_out the filename of output figue,should end up with pdf or png
#' @return NULL
#' @import ggplot2
#' @import RColorBrewer
#' @export
#' @keywords plot_cdr3_stat
#' @examples
#' #' plot_cdr3_stat(stat_matrix,'cdr3aa-distribution-stat.pdf')

plot_cdr3_stat <- function(stat_matrix,file_out){
  require(ggplot2); require(reshape); require(RColorBrewer)
  df= stat_matrix
  df$value <- as.numeric(as.character(df$value))

  # this routine prepares data for donut plot representation
  makesub <- function(t) {
    tmp <- subset(df, type %in% t)
    if(nrow(tmp)==0){
      return(NULL)
    }else{
      tmp$vmax = cumsum(tmp$value)
      tmp$vmin = c(0, head(tmp$vmax, n=-1))
      tmp
    }

  }

  # select color for different arcs
  makecol <- function(d, pal) {
    col <- character(nrow(d))
    p <- colorRampPalette(pal)(nrow(d))

    for (i in 1:nrow(d)) {
      names(col)[i] <- as.character(d$name[i])
      col[i] <- p[i]
    }

    col
  }

  # format three levels of data
  # set - singletons, doubletons & high-order
  # quantile - q1..q5
  # top - top1 clonotype, top2 clonotype...
  df.1 <- makesub("set")
  df.2 <- makesub("quantile")
  df.3 <- makesub("top")

  col <- makecol(df.1, brewer.pal(3,"RdYlBu"))
  col <- c(col, makecol(df.2, rev(brewer.pal(3,"Oranges"))))



  # plotting function for simplicity
  my.plot <- function(...) {
    p=ggplot() +
      geom_rect(data = df.1, colour="grey30", aes(fill=name, ymax=vmax, ymin=vmin, xmax=2,xmin=1)) +
      geom_rect(data = df.2, colour="grey30", aes(fill=name, ymax=vmax, ymin=vmin, xmax=4,xmin=2)) +
      # here we also rotate text to point to right direction
      geom_text(data = df.1, aes(x=1.5, y = value/2 + c(0, cumsum(value)[-length(value)]), angle=180-..y..*360, label = name), size=4)+
      geom_text(data = df.2, aes(x=3.0, y = value/2 + c(0, cumsum(value)[-length(value)]), angle=90-..y..*360, label = name), size=4)
    if(!is.null(df.3)){
      col <- c(col, makecol(df.3, rev(brewer.pal(3,"OrRd"))))
      p=p+   geom_rect(data = df.3, colour="grey30", aes(fill=name, ymax=vmax, ymin=vmin, xmax=7,xmin=4)) +
        geom_text(data = df.3, aes(x=4, y = value/2 + c(0, cumsum(value)[-length(value)]), angle=90-..y..*360, label = name, size = 4), hjust=0.0)

    }
    p=p+
      coord_polar(theta="y") +
      scale_x_continuous(expand=c(0,0),limits=c(0, 7)) +
      scale_fill_manual(values=col)+
      xlab("") + ylab("")+
      theme_bw() +
      theme(panel.grid=element_blank()) +
      theme(panel.border=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(legend.position="none")

    print(p)
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

  if(hasArg(file_out)){
    custom.dev(file_out)
    my.plot()
    dev.off()
  }else{
    my.plot()
  }

}
