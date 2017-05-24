#' saturation analyses for CDR3 sequences
#'
#' By 1  2  3  4  5  6  7  8  9 10 20 30 40 50 60 70 80 90 percent
#' To counts the reads/distinct cdr3aa number/QC/diversity
#'
#' @param IRsample  cdr3aa column is needed, other(V,D,J,cdr3nt,type) are optional.
#' @return saturation_matrix  18 rows and 8 columns
#' @import ggplot2
#' @export
#' @keywords plot_cdr3_stat
#' @examples
#' #' plot_cdr3_stat(stat_matrix,'cdr3aa-distribution-stat.pdf')

stat_cdr3aa_saturation <- function(IRsample){
  require(ggplot2);
  all_reads=nrow(IRsample)
  all_cdr3=length(unique(IRsample$cdr3aa))
  base_reads=floor(all_reads/100)
  len=lapply((c(1:9,1:9*10)), function(i){
    sub_IRsample=IRsample[sample(all_reads, base_reads*i),]

    tmp=sub_IRsample[,4]
    tmp=tmp[tmp!='N/A']
    return(c(
      length(unique(sub_IRsample$cdr3aa)),
      QC_IR(sub_IRsample),
      stat_cdr3_diversity(tmp,'d50'),
      stat_cdr3_diversity(tmp,'shannon'),
      stat_cdr3_diversity(tmp,'simpson'),
      stat_cdr3_diversity(tmp,'gini')
    ))
  })
  saturation=do.call(rbind,len)
  saturation=cbind(paste0(c(1:9,1:9*10),"%"),saturation)
  #saturation=rbind(c('100%',all_cdr3,QC_IR(IRsample),,),saturation)
  colnames(saturation)=c('percent','unique cdr3aa number','all reads','not qualified','not cdr3 aa','d50','shannon','simpson','gini')
  return(saturation)
}
