#'  basic bstatistics for summary table from igblastn result
#'
#' just generate some figures
#'
#' @param stat_IR_basic(IR_basic_df)  the results from stat_IR_basic(IR_basic_df)
#' @return pdf files for V/D/J/CDR3 usage  ~~~~
#' @import plyr
#' @import ggplot
#' @import reshape
#' @export
#' @keywords stat_IR_basic
#' @examples
#' #' plot_IR_basic(stat_IR_basic(IR_basic_df),'test')


plot_IR_basic <- function(this_result,this_sample){

  plot_usage <- function(dat){
    colnames(dat)=c('x','y')
    p=ggplot(dat,aes(x,y))+geom_bar(stat='identity')+ theme_classic() + scale_y_continuous(expand=c(0,0))+
      theme(text=element_text(face='bold'),axis.text.x=element_text(angle=45,hjust=1,size=10),
            axis.title.x=element_blank())+labs(title='Usage',y="percent")
    print(p)
  }

  pdf(paste0(this_sample,".v_usage.pdf"),width = 10)
  plot_usage(this_result$v_usage);dev.off()
  pdf(paste0(this_sample,".d_usage.pdf"))
  plot_usage(this_result$d_usage);dev.off()
  pdf(paste0(this_sample,".j_usage.pdf"))
  plot_usage(this_result$j_usage);dev.off()
  pdf(paste0(this_sample,".cdr3aa_length_usage.pdf"))
  plot_usage(this_result$cdr3aa_length_usage);dev.off()

  plot_cdr3aa_bar(this_result$cdr3aa_length_v, 'V segment' ,
                  file_out =  paste0(this_sample,".cdr3aa_length_v.pdf"))
  plot_cdr3aa_bar(this_result$cdr3aa_length_cdr3aa, 'CDR aa',
                  file_out =  paste0(this_sample,".cdr3aa_length_cdr3aa.pdf"))
  plot_cdr3_stat(this_result$cdr3aa_stat,
                 file_out = paste0(this_sample,".cdr3aa_stat.pdf"))
  plot_v_j_combination(this_result$vj_usage_matrix,image_type = 'circle',
                       file_out = paste0(this_sample,".v_j_circle.pdf"))
  plot_v_j_combination(this_result$vj_usage_matrix,image_type = 'bubble',
                       file_out = paste0(this_sample,".v_j_bubble.pdf"))
}


