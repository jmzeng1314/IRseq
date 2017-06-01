#' plot the t test results of the matrix
#'
#' It will generate a published quality figure according to the V/D/J usage matrix t test results
#'
#' @param t_test_results p/t/type column are needed, others are option
#' @param file_out the filename of output figue,should end up with pdf or png
#' @return NULL
#' @import gplots
#' @import ggplot2
#' @export
#' @keywords heatmap
#' @examples
#' #' plot_vdj_usage_ht(v_usage_matrix,'v-usage-heatmap.pdf')

plot_matrix_t_test <- function(t_test_results,file_out){
 require(ggplot2);
  p=t_test_results$p
  t=t_test_results$t
  p[is.na(p)]=0
  p=-log10(p)
  p[is.infinite(p)]=0
  color_type=c();
  type=t_test_results$type
  for (i in 1:length(p)) {color_type[i]=ifelse(p[i]<1.31,"invariant",ifelse(t[i]>0,"up","down")) }

  # plotting function for simplicity
  my.plot <- function(...) {
    plot_data=data.frame(type,p,change=color_type)
    image=ggplot(plot_data,aes(x=plot_data[,1]))+
      geom_point(aes(y=p,fill=change),size = I(5),shape=21)+
      scale_fill_manual(breaks = c("up", "down", "invariant"),
                         values=c("blue", "green", "red"))
    image=image+theme_set(theme_set(theme_bw(base_size=20)))+
      theme(text=element_text(face='bold'),
            axis.text.x=element_text(angle=45,hjust=1,size=15),
            axis.title.x=element_blank()
            )
    image=image+labs(y="-log(p value)")+
      annotate("text",0.8*length(p),1.5,label="p-value=0.05",size=12)+
      geom_hline(yintercept =1.30103, colour="red",size=2)
    #image=image+scale_x_continuous(breaks=1:30)
    for(i in 1:length(p))
    {
      if (p[i]>1.30103)
        image=image+geom_vline(xintercept =type[i], colour="green",linetype = "longdash",size=1.2)
    }
    return(image)
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
    return(my.plot())
  }

}
