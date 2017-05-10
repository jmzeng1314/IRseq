#load('test/input_files/keeP_IRdata.RData')
#devtools::use_data( keeP_IRdata, overwrite =T)

library(IRseq)
library(ggplot2)
data(keeP_IRdata)

qc=do.call(rbind,lapply(keeP_IRdata,QC_IR))

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
diversity=do.call(rbind,tmp)
colnames(diversity)=c('d50','shannon','simpson','gini')



plot_t_test(diversity[c(1:7,10),1],diversity[c(8:9,11:15),1] )
plot_t_test(diversity[c(1:7,10),2],diversity[c(8:9,11:15),2] )
plot_t_test(diversity[c(1:7,10),3],diversity[c(8:9,11:15),3] )
plot_t_test(diversity[c(1:7,10),4],diversity[c(8:9,11:15),4] )

## basic plot for each sample

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



all_results <- lapply(keeP_IRdata,stat_IR_basic)
plot_cdr3_paired_comparison(all_results$IgAN10$all_cdr3aa_usage , all_results$IgAN12$all_cdr3aa_usage,
                            'IgAN10','IgAN12')
lapply(1:length(all_results), function(i){
  this_result <- all_results[i]
  this_sample <- names(all_results)[i]

  pdf(paste0(this_sample,".v_usage.pdf"))
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
})

lapply(1:length(all_results), function(i){
  this_result <- all_results[i]
  this_sample <- names(all_results)[i]

})


