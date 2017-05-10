#' distribution of V/D/J usage or clonetype according to length of CDR3 aa
#'
#' It will generate a published quality figure according to
#' V/d/j or clonetype and length of CDR3 aa
#'
#' @param input_matrix Firstly column should be length of CDR3 aa
#' @param label clonetype or V/D/J segment
#' @param file_out the filename of output figue,should end up with pdf or png

#' @return NULL
#' @import ggplot2
#' @import reshape
#' @import RColorBrewer
#' @export
#' @keywords circle
#' @examples
#' #' plot_cdr3aa_bar(input_matrix,,'Variable segment','v-usage-cdr3aa-length.pdf' )

plot_cdr3aa_bar <- function(input_matrix,label,file_out){
  gradient=F
  require(ggplot2); require(reshape); require(RColorBrewer)
  df=input_matrix

  df[, 1:ncol(df)] <- apply(df[, 1:ncol(df)], 2, as.numeric)
  colnames(df)[1]='Len';df$Len
  df.m <- melt(df, id = "Len")

  # plotting function for simplicity
  my.plot <- function(...) {

    # custom palette (color blind)
    pal=''
    if (gradient) {
      pal <- colorRampPalette(c("#2b8cbe", "#e0f3db", "#fdbb84"))(ncol(df) - 2)
    } else {
      pal <- brewer.pal(ncol(df) - 2, "Paired")
    }

    p=ggplot(df.m, aes(x = Len, y = value, fill = variable)) +
      geom_bar(width = 1, stat = "identity") +
      xlab("CDR3 length, bp") +
      labs(fill=label) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_manual(values=c("grey75", pal)) +
      theme_bw() +
      theme(legend.text=element_text(size=8), axis.title.y=element_blank(),
      axis.text.x=element_text(angle=45,hjust=1,size=10),
      panel.grid.major=element_blank()) +
      guides(fill = guide_legend(reverse = TRUE))

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
