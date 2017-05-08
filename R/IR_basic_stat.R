#'  basic bstatistics for summary table from igblastn result
#'
#' analysis the summary table from igblastn result, which must  includes the following fields:
#' Variable (V) segment name.
#' Diversity (D) segment name for some of the receptor chains (TRB, TRD and IGH). Set to . if not aplicable or D segment was not identified.
#' Joining (J) segment name.
#' Complementarity determining region 3 nucleotide sequence (CDR3nt). CDR3 starts with Variable region reference point (conserved Cys residue) and ends with Joining segment reference point (conserved PheTry).
#' Translated CDR3 sequence (CDR3aa).
#' THe other columns are optional: type,~~~
#'
#'
#' @param IR_basic_df
#' @return A list contain:v/d/j usage, top 10 cdr3,
#' @import plyr
#' @import ggplot
#' @export
#' @keywords IR_basic_stat
#' @examples
#' #' IR_basic_stat(IR_basic_df)

IR_basic_stat <- function(IR_basic_df){
  library(dplyr)
  ## firtly for v/d/j segment usage:
  v_usage=ddply(IR_basic_df,.(V),nrow)
  v_usage$V1=v_usage$V1/sum(v_usage$V1)
  d_usage=ddply(IR_basic_df,.(D),nrow)
  d_usage$V1=d_usage$V1/sum(d_usage$V1)
  j_usage=ddply(IR_basic_df,.(J),nrow)
  j_usage$V1=j_usage$V1/sum(j_usage$V1)

  ## secondly for v-j combination
  tmp=table(IR_basic_df[,c('V','J')])
  vj_usage_matrix=as.matrix(tmp) ## ddply(IR_basic_df,.(V,J),nrow)
  vj_usage_matrix[,3]= vj_usage_matrix[,3]/sum( vj_usage_matrix[,3])
  #head(vj_usage_matrix)

  #v_j_combination(vj_usage_matrix,'bubble','tmp.pdf')

  ## thirdly for cdr3 length
  cdr3_IR <- subset(IR_basic_df,cdr3aa != 'N/A')

  tmp=sort(table(cdr3_IR$cdr3aa),decreasing = T)
  all_cdr3aa_usage=data.frame(cdr3aa=names(tmp),
                        percent=as.numeric(tmp)/sum(as.numeric(tmp)),
                        stringsAsFactors = F)

  set1=sum(all_cdr3aa_usage$percent[all_cdr3aa_usage$percent == 1/nrow(cdr3_IR)])
  set2=sum(all_cdr3aa_usage$percent[all_cdr3aa_usage$percent == 2/nrow(cdr3_IR)])
  set3=1-set1-set2


  top10_cdr3aa=head(all_cdr3aa_usage,10)

  cdr3_IR$cdr3aa_length=nchar(cdr3_IR$cdr3aa)
  cdr3aa_length_usage=ddply(cdr3_IR,.(cdr3aa_length),nrow)
  cdr3aa_length_usage[,2]=cdr3aa_length_usage[,2]/sum(cdr3aa_length_usage[,2])

  cdr3aa_length_cdr3aa <- ddply(cdr3_IR,.(cdr3aa_length),function(x){
    tmp=table(x$cdr3aa)
    tmp=as.numeric(tmp[top10_cdr3aa$cdr3aa])
    tmp[is.na(tmp)]=0
    other=nrow(x)-sum(tmp)
    counts=c(other,tmp)
    return(counts/sum(counts))
  })
  colnames(cdr3aa_length_cdr3aa)=c('len','other',top10_cdr3aa$cdr3aa)
  #barplot_cdr3aa(cdr3aa_length_cdr3aa,'cdr3aa_length_cdr3aa.pdf','CDR3 AA','F')


  top10v=head(v_usage[order(v_usage$V1,decreasing = T),1],10)
  cdr3aa_length_v <- ddply(cdr3_IR,.(cdr3aa_length),function(x){
    tmp=table(x$V)
    tmp=as.numeric(tmp[top10v])
    tmp[is.na(tmp)]=0
    other=nrow(x)-sum(tmp)
    counts=c(other,tmp)
    return(counts/sum(counts))
  })
  colnames(cdr3aa_length_v)=c('len','other',top10v)
  #barplot_cdr3aa(cdr3aa_length_v,'cdr3aa_length_v.pdf','V segment','F')

  ## cdr3aa quantile statistics :


}
