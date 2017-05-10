---
title: "IRseq example"
author: "jmzeng1314@163.com"
date: "May 10, 2017"
output: html_document
---

## load package and data



```r
library(IRseq) 
library(ggplot2)
load('../../keep_IRdata.RData')
names(keep_IRdata)
```

```
##  [1] "IgAN10" "IgAN11" "IgAN12" "IgAN13" "IgAN14" "IgAN1"  "IgAN4" 
##  [8] "NC1"    "NC2"    "IgAN8"  "NC3"    "NC4"    "NC5"    "NC6"   
## [15] "NC7"
```

```r
head(keep_IRdata[[1]])
```

```
##          V        D     J          cdr3aa
## 1 IGHV1-69      N/A IGHJ2             N/A
## 2 IGHV5-51 IGHD3-16 IGHJ4         ARPIGGS
## 3 IGHV5-51 IGHD6-19 IGHJ6 ARASGSPGYYYGLDV
## 4 IGHV3-11 IGHD3-10 IGHJ4     AMGMNSGPFDY
## 5 IGHV1-69      N/A IGHJ2             N/A
## 6 IGHV1-69 IGHD5-12 IGHJ4                
##                                          cdr3nt type
## 1                                           N/A  IgG
## 2                         TGTGCGAGACCCATAGGGGGA  IgA
## 3 TGTGCGAGGGCCAGTGGCTCCCCAGGCTACTACTATGGTCTGGAC  IgA
## 4             TGTGCGATGGGGATGAATTCGGGACCCTTTGAC  IgM
## 5                                           N/A  IgG
## 6                                                IgA
```

## basic QC for all of the samples


```r
qc=do.call(rbind,lapply(keep_IRdata,QC_IR))
colnames(qc)=c('all reads','not qualified','not cdr3 aa')
qc
```

```
##        all reads not qualified not cdr3 aa
## IgAN10    555245   0.237372691   15.505408
## IgAN11    485496   0.567254931    5.311063
## IgAN12    503726   0.503051262    5.896658
## IgAN13   1472111   0.038991625    8.408469
## IgAN14   1398189   0.074095848    8.858388
## IgAN1     432619   0.259119456    7.139307
## IgAN4     833151   0.911719484    5.625871
## NC1       115656   6.080099606   25.173791
## NC2       127082   3.832958247   12.897184
## IgAN8     525198   0.674983530   15.788712
## NC3        53357   0.001874168    4.818487
## NC4       192382   0.266656964   17.742824
## NC5       396471   0.015385741    4.422265
## NC6        65930   0.003033520    5.235856
## NC7       202129   0.006926270    4.441718
```

## basic statistic for cdr3 diveristy


```r
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
diversity
```

```
##                d50   shannon   simpson      gini
## IgAN10 0.001092598  8.213754 0.9700427 0.9331964
## IgAN11 0.001718895 11.252913 0.9909669 0.7915374
## IgAN12 0.003280135 10.853192 0.9740431 0.8332598
## IgAN13 0.005226064 13.825375 0.9982191 0.7852477
## IgAN14 0.008561207 14.973048 0.9976903 0.6171281
## IgAN1  0.004185775 11.239450 0.9939039 0.8289348
## IgAN4  0.015150694 13.754278 0.9993747 0.7973730
## NC1    0.043169562 12.900782 0.9957020 0.5708877
## NC2    0.130368453 14.347130 0.9981629 0.4117261
## IgAN8  0.002877341 10.613797 0.9925154 0.8625730
## NC3    0.356120150 14.739140 0.9996271 0.2118628
## NC4    0.006001963 11.304577 0.9957215 0.7568130
## NC5    0.017364477 14.408184 0.9991837 0.5919746
## NC6    0.280238597 14.647696 0.9996200 0.2920078
## NC7    0.301749219 16.386625 0.9997372 0.2609722
```


> we can do t.test for cdr3 diversity between case and control group


```r
## for d50 index:
plot_t_test(diversity[c(1:7,10),1],diversity[c(8:9,11:15),1] )
```

<img src="IRseq_example_files/figure-html/unnamed-chunk-4-1.png" width="672" />

```r
## for shanon entropy
plot_t_test(diversity[c(1:7,10),2],diversity[c(8:9,11:15),2] )
```

<img src="IRseq_example_files/figure-html/unnamed-chunk-4-2.png" width="672" />

```r
# for simpson index
plot_t_test(diversity[c(1:7,10),3],diversity[c(8:9,11:15),3] )
```

<img src="IRseq_example_files/figure-html/unnamed-chunk-4-3.png" width="672" />

```r
## for gini coefficient 
plot_t_test(diversity[c(1:7,10),4],diversity[c(8:9,11:15),4] )
```

<img src="IRseq_example_files/figure-html/unnamed-chunk-4-4.png" width="672" />

> also we can generate publication qualited figures


```r
plot_t_test(diversity[c(1:7,10),1],diversity[c(8:9,11:15),1] ,'d50.t.test.png')
plot_t_test(diversity[c(1:7,10),2],diversity[c(8:9,11:15),2] ,'shannon.t.test.png')
plot_t_test(diversity[c(1:7,10),3],diversity[c(8:9,11:15),3] ,'simpson.t.test.png')
plot_t_test(diversity[c(1:7,10),4],diversity[c(8:9,11:15),4] ,'gini.t.test.png')
```

## basic statistcs for v/d/j/cdr3 usage for each sample

> choose the first sample as a example 


```r
IR_basic_df=keep_IRdata[[1]]
IR_basic_stat_results <- stat_IR_basic(IR_basic_df)
```

```
## 
## Attaching package: 'reshape'
```

```
## The following objects are masked from 'package:plyr':
## 
##     rename, round_any
```

```r
str(IR_basic_stat_results)
```

```
## List of 11
##  $ cdr3aa_diversity    : num [1:4] 0.00155 8.58043 0.97459 0.92621
##  $ v_usage             :'data.frame':	64 obs. of  2 variables:
##   ..$ V : chr [1:64] "IGHV1-18" "IGHV1-2" "IGHV1-24" "IGHV1-3" ...
##   ..$ V1: num [1:64] 0.192814 0.001072 0.01376 0.009633 0.000321 ...
##  $ d_usage             :'data.frame':	30 obs. of  2 variables:
##   ..$ D : chr [1:30] "IGHD1-1" "IGHD1-14" "IGHD1-20" "IGHD1-26" ...
##   ..$ V1: num [1:30] 0.02059 0.01774 0.00292 0.03916 0.00646 ...
##  $ j_usage             :'data.frame':	7 obs. of  2 variables:
##   ..$ J : chr [1:7] "IGHJ1" "IGHJ2" "IGHJ3" "IGHJ4" ...
##   ..$ V1: num [1:7] 0.00703 0.03298 0.0306 0.57269 0.07191 ...
##  $ all_cdr3aa_usage    :'data.frame':	21965 obs. of  2 variables:
##   ..$ cdr3aa : chr [1:21965] "ARPIGGS" "AMGMNSGPFDY" "AKSKAEHYFDY" "ARASGSPGYYYGLDV" ...
##   ..$ percent: num [1:21965] 0.136 0.0424 0.032 0.028 0.0219 ...
##  $ cdr3aa_length_usage :'data.frame':	44 obs. of  2 variables:
##   ..$ cdr3aa_length: int [1:44] 2 3 4 5 6 7 8 9 10 11 ...
##   ..$ V1           : num [1:44] 4.26e-06 5.54e-05 4.05e-05 6.39e-03 3.42e-02 ...
##  $ top10_cdr3aa        :'data.frame':	10 obs. of  2 variables:
##   ..$ cdr3aa : chr [1:10] "ARPIGGS" "AMGMNSGPFDY" "AKSKAEHYFDY" "ARASGSPGYYYGLDV" ...
##   ..$ percent: num [1:10] 0.136 0.0424 0.032 0.028 0.0219 ...
##  $ cdr3aa_length_v     :'data.frame':	44 obs. of  12 variables:
##   ..$ len      : int [1:44] 2 3 4 5 6 7 8 9 10 11 ...
##   ..$ other    : num [1:44] 2.13e-06 4.26e-06 0.00 5.18e-04 2.69e-03 ...
##   ..$ IGHV5-51 : num [1:44] 0 0 0 0.000117 0.000213 ...
##   ..$ IGHV1-18 : num [1:44] 0.00 2.13e-06 2.13e-06 4.01e-04 4.01e-04 ...
##   ..$ IGHV3-11 : num [1:44] 0.00 4.48e-05 3.20e-05 1.39e-03 2.69e-02 ...
##   ..$ IGHV7-4-1: num [1:44] 2.13e-06 0.00 0.00 1.64e-04 1.66e-04 ...
##   ..$ IGHV3-30 : num [1:44] 0.00 2.13e-06 4.26e-06 3.03e-03 8.65e-04 ...
##   ..$ IGHV1-69 : num [1:44] 0 0 0 0.000311 0.000143 ...
##   ..$ IGHV4-34 : num [1:44] 0.00 0.00 0.00 5.12e-05 6.99e-04 ...
##   ..$ IGHV3-48 : num [1:44] 0.00 0.00 2.13e-06 6.39e-05 1.72e-03 ...
##   ..$ IGHV1-24 : num [1:44] 0.00 2.13e-06 0.00 2.75e-04 5.33e-05 ...
##   ..$ IGHV1-46 : num [1:44] 0.00 0.00 0.00 6.82e-05 3.82e-04 ...
##  $ cdr3aa_stat         :'data.frame':	20 obs. of  3 variables:
##   ..$ type : chr [1:20] "set" "set" "set" "quantile" ...
##   ..$ name : chr [1:20] "3+" "2" "1" "Q1" ...
##   ..$ value: num [1:20] 0.96088 0.00881 0.03031 0.95215 0.01976 ...
##  $ vj_usage_matrix     : num [1:7, 1:64] 0.000941 0.006752 0.002464 0.117978 0.010655 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:7] "IGHJ1" "IGHJ2" "IGHJ3" "IGHJ4" ...
##   .. ..$ : chr [1:64] "IGHV1-18" "IGHV1-2" "IGHV1-24" "IGHV1-3" ...
##  $ cdr3aa_length_cdr3aa:'data.frame':	44 obs. of  12 variables:
##   ..$ len            : int [1:44] 2 3 4 5 6 7 8 9 10 11 ...
##   ..$ other          : num [1:44] 4.26e-06 5.54e-05 4.05e-05 6.39e-03 1.47e-02 ...
##   ..$ ARPIGGS        : num [1:44] 0 0 0 0 0 ...
##   ..$ AMGMNSGPFDY    : num [1:44] 0 0 0 0 0 ...
##   ..$ AKSKAEHYFDY    : num [1:44] 0 0 0 0 0 ...
##   ..$ ARASGSPGYYYGLDV: num [1:44] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ ARVNGSPGYFYGMDV: num [1:44] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ SKGGDY         : num [1:44] 0 0 0 0 0.0196 ...
##   ..$ ARVTSDYGDYAEY  : num [1:44] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ ARPIGGP        : num [1:44] 0 0 0 0 0 ...
##   ..$ ARSGLDYNTLYLMDV: num [1:44] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ ARSGLDYNNLYLMDV: num [1:44] 0 0 0 0 0 0 0 0 0 0 ...
```

```r
names(IR_basic_stat_results)
```

```
##  [1] "cdr3aa_diversity"     "v_usage"              "d_usage"             
##  [4] "j_usage"              "all_cdr3aa_usage"     "cdr3aa_length_usage" 
##  [7] "top10_cdr3aa"         "cdr3aa_length_v"      "cdr3aa_stat"         
## [10] "vj_usage_matrix"      "cdr3aa_length_cdr3aa"
```

```r
plot_usage <- function(dat){
  colnames(dat)=c('x','y')
  p=ggplot(dat,aes(x,y))+geom_bar(stat='identity')+ theme_classic() + scale_y_continuous(expand=c(0,0))+
    theme(text=element_text(face='bold'),axis.text.x=element_text(angle=45,hjust=1,size=10),
          axis.title.x=element_blank())+labs(title='Usage',y="percent")
  print(p)
}
plot_usage(IR_basic_stat_results$v_usage)
```

<img src="IRseq_example_files/figure-html/unnamed-chunk-6-1.png" width="672" />

```r
plot_usage(IR_basic_stat_results$d_usage)
```

<img src="IRseq_example_files/figure-html/unnamed-chunk-6-2.png" width="672" />

```r
plot_usage(IR_basic_stat_results$j_usage)
```

<img src="IRseq_example_files/figure-html/unnamed-chunk-6-3.png" width="672" />

```r
plot_usage(IR_basic_stat_results$cdr3aa_length_usage)
```

<img src="IRseq_example_files/figure-html/unnamed-chunk-6-4.png" width="672" />

```r
plot_cdr3aa_bar(IR_basic_stat_results$cdr3aa_length_v, 'V segment' )
```

```
## Loading required package: RColorBrewer
```

<img src="IRseq_example_files/figure-html/unnamed-chunk-6-5.png" width="672" />

```r
plot_cdr3aa_bar(IR_basic_stat_results$cdr3aa_length_cdr3aa, 'CDR aa' )
```

<img src="IRseq_example_files/figure-html/unnamed-chunk-6-6.png" width="672" />

```r
plot_cdr3_stat(IR_basic_stat_results$cdr3aa_stat)
```

<img src="IRseq_example_files/figure-html/unnamed-chunk-6-7.png" width="672" />

```r
plot_v_j_combination(IR_basic_stat_results$vj_usage_matrix,image_type = 'circle',file_out = 'test.v-j.circle.pdf')
```

```
## Loading required package: circlize
```

```
## Warning: package 'circlize' was built under R version 3.3.3
```

```
## png 
##   2
```

```r
plot_v_j_combination(IR_basic_stat_results$vj_usage_matrix,image_type = 'bubble')
```

<img src="IRseq_example_files/figure-html/unnamed-chunk-6-8.png" width="672" />

## stat for all of the samples:

```r
all_results <- lapply(keep_IRdata,stat_IR_basic)
```
> then you can choose two of them to campare their shared CDR3

```r
plot_cdr3_paired_comparison(all_results$IgAN10$all_cdr3aa_usage , all_results$IgAN12$all_cdr3aa_usage,
                            'IgAN10','IgAN12')
```

```
## Loading required package: grid
```

```
## Loading required package: gridExtra
```

```
## Warning: Ignoring unknown aesthetics: weight

## Warning: Ignoring unknown aesthetics: weight

## Warning: Ignoring unknown aesthetics: weight

## Warning: Ignoring unknown aesthetics: weight
```

```
## Warning: Removed 66987 rows containing non-finite values (stat_density).
```

```
## Warning: Computation failed in `stat_density()`:
## 'weights' must all be finite
```

```
## Warning: Removed 87955 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 87955 rows containing missing values (geom_point).
```

```
## Warning: Removed 20968 rows containing non-finite values (stat_density).
```

```
## Warning: Computation failed in `stat_density()`:
## 'weights' must all be finite
```

<img src="IRseq_example_files/figure-html/unnamed-chunk-8-1.png" width="672" />

> but It's a little complicated, just for a example.

## generate all of the figures for all of the samples
 

```r
lapply(1:length(all_results), function(i){
  this_result <- all_results[[i]]
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
```


## subset the IR data by IG subtype: IgG,IgA,IgM

> The do the same process as the whole IR data :


```r
all_results_IgA <- lapply(keep_IRdata,function(x){
  x=subset(x,type='IgA')
  stat_IR_basic(x)
})

lapply(1:length(all_results_IgA), function(i){
  this_result <- all_results_IgA[[i]]
  this_sample <- names(all_results_IgA)[i]
  this_sample <- paste0(this_sample,".IgA")
  pdf(paste0(this_sample,".v_usage.pdf"))
  plot_usage(this_result$v_usage);
  dev.off()
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


all_results_IgM <- lapply(keep_IRdata,function(x){
  x=subset(x,type='IgM')
  stat_IR_basic(x)
})
lapply(1:length(all_results_IgM), function(i){
  this_result <- all_results_IgM[[i]]
  this_sample <- names(all_results_IgM)[i]
  this_sample <- paste0(this_sample,".IgM")
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


all_results_IgG <- lapply(keep_IRdata,function(x){
  x=subset(x,type='IgG')
  stat_IR_basic(x)
})
lapply(1:length(all_results_IgG), function(i){
  this_result <- all_results_IgG[[i]]
  this_sample <- names(all_results_IgG)[i]
  this_sample <- paste0(this_sample,".IgG")
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

save(all_results,all_results_IgA,all_results_IgG,all_results_IgM,file='analysis_result.Rdata')
```


## compare the v/j/usage and between case and control group 





