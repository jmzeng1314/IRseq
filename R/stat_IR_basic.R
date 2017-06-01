#'  basic bstatistics for summary table from igblastn result
#'
#' analysis the summary table from igblastn result, v/d/j/cdr3aa/
#' THe other columns are optional: type,~~~
#' cdr3aa_diversity=cdr3aa_diversity,
#' v_usage=v_usage,
#' d_usage=d_usage,
#' j_usage=j_usage,
#' all_cdr3aa_usage=all_cdr3aa_usage,
#' cdr3aa_length_usage=cdr3aa_length_usage,
#' top10_cdr3aa=top10_cdr3aa,
#' cdr3aa_length_v=cdr3aa_length_v,
#' cdr3aa_stat=cdr3aa_stat,
#' vj_usage_matrix=vj_usage_matrix,
#' cdr3aa_length_cdr3aa=cdr3aa_length_cdr3aa
#'
#'
#' @param IR_basic_df
#' @return A list contain:v/d/j usage, top 10 cdr3,
#' @import plyr
#' @import ggplot
#' @import reshape
#' @export
#' @keywords stat_IR_basic
#' @examples
#' #' stat_IR_basic(IR_basic_df)

stat_IR_basic <- function(IR_basic_df){
  library(plyr)
  library(reshape)
  ## firtly for v/d/j segment usage:
  IR_basic_df= subset(IR_basic_df,V !='N/A' & V != '')
  v_usage=ddply(IR_basic_df,.(V),nrow)
  v_usage$V1=v_usage$V1/sum(v_usage$V1)
  d_usage=ddply(IR_basic_df,.(D),nrow)
  d_usage$V1=d_usage$V1/sum(d_usage$V1)
  j_usage=ddply(IR_basic_df,.(J),nrow)
  j_usage$V1=j_usage$V1/sum(j_usage$V1)

  ## secondly for v-j combination

  tmp=ddply(IR_basic_df,.(V,J),nrow)
  vj_usage_matrix = cast(tmp,V~J,value='V1',fill=0)
  rownames(vj_usage_matrix)=vj_usage_matrix[,1]
  vj_usage_matrix=vj_usage_matrix[,-1]
  vj_usage_matrix=vj_usage_matrix/sum(vj_usage_matrix)
  vj_usage_matrix=t(vj_usage_matrix)
  ## please be sure that cols are V segments and rows are J segments
  #head(vj_usage_matrix)

  #v_j_combination(vj_usage_matrix,'bubble','tmp.pdf')

  ## thirdly for cdr3 length
  cdr3_IR <- subset(IR_basic_df,cdr3aa != 'N/A' & cdr3aa != '')

  tmp=sort(table(cdr3_IR$cdr3aa),decreasing = T)
  all_cdr3aa_usage=data.frame(cdr3aa=names(tmp),
                        percent=as.numeric(tmp)/sum(as.numeric(tmp)),
                        stringsAsFactors = F)
  p=all_cdr3aa_usage$percent
  set1=sum(p[p== 1/nrow(cdr3_IR)])
  set2=sum(p[p== 2/nrow(cdr3_IR)])
  set3=1-set1-set2
  five=floor(length(p)/5)
  Q1=sum(p[1:five])
  Q2=sum(p[five:(2*five)])
  Q3=sum(p[(2*five):(3*five)])
  Q4=sum(p[(3*five):(4*five)])
  Q5=sum(p[(4*five):length(p)])
  top_cdr3aa=subset(all_cdr3aa_usage,percent>0.01)

  cdr3aa_stat<-data.frame(type=c(rep('set',3),rep('quantile',5),rep('top',nrow(top_cdr3aa))),
                          name=c('3+',2,1,'Q1','Q2','Q3','Q4','Q5',top_cdr3aa$cdr3aa),
                          value=c(set3,set2,set1,Q1,Q2,Q3,Q4,Q5,top_cdr3aa$percent),
                          stringsAsFactors = F
                          )
  #cdr3aa_stat
  ## maybe something is wrong for quantile stat

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
    return(counts/ nrow(cdr3_IR))
  })
  colnames(cdr3aa_length_cdr3aa)=c('len','other',top10_cdr3aa$cdr3aa)
  #plot_cdr3aa_bar(cdr3aa_length_cdr3aa, 'CDR3 aa' )


  top10v=head(v_usage[order(v_usage$V1,decreasing = T),1],10)
  cdr3aa_length_v <- ddply(cdr3_IR,.(cdr3aa_length),function(x){
    tmp=table(x$V)
    tmp=as.numeric(tmp[top10v])
    tmp[is.na(tmp)]=0
    other=nrow(x)-sum(tmp)
    counts=c(other,tmp)
    return(counts/ nrow(cdr3_IR))
  })
  colnames(cdr3aa_length_v)=c('len','other',top10v)
  #plot_cdr3aa_bar(cdr3aa_length_v, 'V segment' )


  ## CDR3 aa diversity :
  tmp=cdr3_IR$cdr3aa
  cdr3aa_diversity <- c(
    stat_cdr3_diversity(tmp,'d50'),
    stat_cdr3_diversity(tmp,'shannon'),
    stat_cdr3_diversity(tmp,'simpson'),
    stat_cdr3_diversity(tmp,'gini')
  )

  return(list(
    cdr3aa_diversity=cdr3aa_diversity,
    v_usage=v_usage,
    d_usage=d_usage,
    j_usage=j_usage,
    all_cdr3aa_usage=all_cdr3aa_usage,
    cdr3aa_length_usage=cdr3aa_length_usage,
    top10_cdr3aa=top10_cdr3aa,
    cdr3aa_length_v=cdr3aa_length_v,
    cdr3aa_stat=cdr3aa_stat,
    vj_usage_matrix=vj_usage_matrix,
    cdr3aa_length_cdr3aa=cdr3aa_length_cdr3aa
  ))
}
