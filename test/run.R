#load('test/input_files/keeP_IRdata.RData')
#devtools::use_data( keeP_IRdata, overwrite =T)
library(IRseq)
data(keeP_IRdata)
keeP_IRdata=keep_IRdata

do.call(rbind,lapply(keeP_IRdata,QC_IR))

tmp<-lapply(keeP_IRdata,function(x){
  tmp=x[,4]
  tmp=tmp[tmp!='N/A']
  return(c(
    stat_cdr3_diversity(tmp,'d50'),
    stat_cdr3_diversity(tmp,'shannon'),
    stat_cdr3_diversity(tmp,'simpson'),
    stat_cdr3_diversity(tmp,'gini')
  ))
})
do.call(rbind,tmp)

IR_basic_df=keeP_IRdata[[1]]
IR_basic_stat_results <- stat_IR_basic(IR_basic_df)
str(IR_basic_stat_results)
names(IR_basic_stat_results)

plot_usage <- function(dat){
  colnames(dat)=c('x','y')
  p=ggplot(dat,aes(x,y))+geom_bar(stat='identity')+ theme_classic() + scale_y_continuous(expand=c(0,0))+
    theme(text=element_text(face='bold'),axis.text.x=element_text(angle=45,hjust=1,size=10),
          axis.title.x=element_blank())+labs(title='Usage',y="percent")
  print(p)
}
plot_usage(IR_basic_stat_results$v_usage)
plot_usage(IR_basic_stat_results$d_usage)
plot_usage(IR_basic_stat_results$j_usage)
plot_usage(IR_basic_stat_results$cdr3aa_length_usage)
plot_cdr3aa_bar(IR_basic_stat_results$cdr3aa_length_v, 'V segment' )
plot_cdr3aa_bar(IR_basic_stat_results$cdr3aa_length_cdr3aa, 'CDR aa' )
plot_cdr3_stat(IR_basic_stat_results$cdr3aa_stat)
plot_v_j_combination(IR_basic_stat_results$vj_usage_matrix,image_type = 'circle',file_out = 'test.v-j.circle.pdf')
plot_v_j_combination(IR_basic_stat_results$vj_usage_matrix,image_type = 'bubble')

all_results <- lapply(keeP_IRdata,stat_IR_basic)
plot_cdr3_paired_comparison(all_results$case1$all_cdr3aa_usage , all_results$control1$all_cdr3aa_usage,
                            'case','control')




