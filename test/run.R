#load('test/input_files/keeP_IRdata.RData')
#devtools::use_data( keeP_IRdata, overwrite =T)

library(IRseq)
library(ggplot2)
#data(keeP_IRdata)
load('../IRdata.RData')
names(IRdata)
keep_IRdata=IRdata[c(1:5,9,12:20)]
##################################################################
########### basic QC for all of the samples ###################
##################################################################
qc=do.call(rbind,lapply(keep_IRdata,QC_IR))
colnames(qc)=c('all reads','not qualified','not cdr3 aa')
qc=as.data.frame(qc)
qc$sampleID=rownames(qc)
write.csv(merge(qc,config,by='sampleID'),'qc.csv')

##################################################################
########### basic statistic for cdr3 diveristy ###################
##################################################################
tmp<-lapply(keep_IRdata,function(x){
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
diversity=as.data.frame(diversity)
diversity$sampleID=rownames(diversity)
write.csv(merge(diversity,config,by='sampleID'),'diversity.csv')

names(keep_IRdata)
a=diversity[grepl('Ig',names(keep_IRdata)),]
b=diversity[grepl('NC',names(keep_IRdata)),]
plot_t_test(a[,1],b[,1],'d50.t.test.png')
plot_t_test(a[,2],b[,2],'shannon.t.test.png')
plot_t_test(a[,3],b[,3],'simpson.t.test.png')
plot_t_test(a[,4],b[,4],'gini.t.test.png')

## then for cdr3 diveristy of different Ig subtype
lapply(c('IgA','IgM','IgG'),function(this_type){
  old_dir=getwd()
  new_dir=paste0('CDR3_diversity_',this_type)
  dir.create(new_dir)
  setwd(new_dir)

  tmp<-lapply(keep_IRdata,function(x){

    x=subset(x,type=this_type)
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
  diversity=as.data.frame(diversity)
  diversity$sampleID=rownames(diversity)
  write.csv(merge(diversity,config,by='sampleID'),'diversity.csv')

  names(keep_IRdata)
  a=diversity[grepl('Ig',names(keep_IRdata)),]
  b=diversity[grepl('NC',names(keep_IRdata)),]
  plot_t_test(a[,1],b[,1],'d50.t.test.png')
  plot_t_test(a[,2],b[,2],'shannon.t.test.png')
  plot_t_test(a[,3],b[,3],'simpson.t.test.png')
  plot_t_test(a[,4],b[,4],'gini.t.test.png')
  setwd(old_dir)
})

##################################################################
############### analysis based on IgA/G/M subtype ################
##################################################################
type_results <- lapply(keep_IRdata,function(x) table(x$type))
type_counts <- do.call(rbind,type_results)
write.csv(type_counts,'IG_subtype_counts.csv')
tmp=type_counts[,-6]
type_freq <- t(apply(tmp,1,function(x) x/sum(x)))
write.csv(type_freq,'IG_subtype_freq.csv')

a=type_freq[grepl('Ig',rownames(type_freq)),]
b=type_freq[grepl('NC',rownames(type_freq)),]
laply(colnames(type_freq),function(x){
  plot_t_test(a[,x],b[,x],paste0(x,'.t.test.png'))
})

library(reshape)
df=melt(type_freq)
colnames(df)=c('sample','subtype','freq')
df$group=ifelse(grepl('Ig',df$sample),'IgAN','NC')
pie <- ggplot(df, aes(x = "", y=freq, fill = factor(subtype))) +
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(),
        plot.title = element_text(hjust=0.5)) +
  facet_grid(group~.)+
  labs(fill="subtype",
       x=NULL,
       y=NULL,
       title="Pie Chart of sample",
       caption="Source: mpg")

pie + coord_polar(theta = "y", start=0)


##################################################################
########### saturation analyses for CDR3 sequences    ############
##################################################################
# By rarefaction analysis (random resampling of increasingly larger subsets of the data)
# in addition to rarefaction, we used accumulation analysis
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3083096/
IRsample=keep_IRdata$IgAN13
str(IRsample)
saturation_1=stat_cdr3aa_saturation(IRsample)
saturation_2=stat_cdr3aa_saturation(IRsample,10,50)
write.csv(saturation_1,"IgAN13_saturation_1.csv")
write.csv(saturation_2,"IgAN13_saturation_2.csv")

##################################################################
#####  All of the basic stat for V/D/j/CDR3 usage and length######
##################################################################
all_results <- lapply(keep_IRdata,stat_IR_basic)
group_list <- config$group[c(1:5,9,12:20)]
v_comp_results<-compare_usage(all_results,'v_usage',group_list)
j_comp_results<-compare_usage(all_results,'j_usage',group_list)
d_comp_results<-compare_usage(all_results,'d_usage',group_list)
cdr3aa_comp_results<-compare_usage(all_results,'cdr3aa_length_usage',group_list)


plot(cdr3aa_comp_results$p2+xlim(0,30))+scale_x_continuous(breaks = 1:30)
plot(v_comp_results$p2)
plot(j_comp_results$p2)



##################################################################
############  All of the basic stat for IgA subtype ##############
##################################################################
all_results_IgA <- lapply(keep_IRdata,function(x){
  x=subset(x,type='IgA')
  stat_IR_basic(x)
})

##################################################################
############  All of the basic stat for IgG subtype ##############
##################################################################
all_results_IgG <- lapply(keep_IRdata,function(x){
  x=subset(x,type='IgG')
  stat_IR_basic(x)
})

##################################################################
############  All of the basic stat for IgM subtype ##############
##################################################################
all_results_IgM <- lapply(keep_IRdata,function(x){
  x=subset(x,type='IgM')
  stat_IR_basic(x)
})

save.image('tmp.Rdata')




