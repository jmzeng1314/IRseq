#'  compare the V/D/J/cdr3aa length distribution
#'
#'  bar plot and t.test results
#'
#' @param IRseq_results A list contains all the samples' IRseq basic stat results
#' @param choose v_usage/d_usage/j_usage/cdr3aa_length_usage/
#' @param group_list  A vectors,eg 0,0,0,1,1,1,which should correspond to the samples in IRseq_results
#' @return 3 figures and 2 data.frame(dat and stat)
#' @export
#' @keywords compare_usage
#' @examples
#' #' compare_usage(all_results,c(0,0,0,1,1,1))

compare_usage <- function(IRseq_results,choose,group_list){

  tmp <- lapply(1:length(IRseq_results), function(i) {
    tmp=IRseq_results[[i]][[choose]]
    tmp$sample=names(IRseq_results)[i]
    tmp$group=group_list[i]
    tmp
  })
  v_matrix<-do.call(rbind,tmp)
  colnames(v_matrix)=c('type','value','sample','group')
  library(ggplot2)
  p1 <- ggplot(v_matrix,aes(x=type,y=value,color=sample))+geom_bar(stat = 'identity', position = "dodge")+facet_grid(group ~ .)+
    #theme_classic() +
    scale_y_continuous(expand=c(0,0))+
    theme(text=element_text(face='bold'),axis.text.x=element_text(angle=45,hjust=1,size=10),
          axis.title.x=element_blank())+labs(title='Usage',y="percent")
  ## return p_usage_comparison
  p2 <- ggplot(v_matrix,aes(x=type,y=value ,fill=group))+geom_bar(stat = 'identity', position = "dodge")+
    facet_grid(sample~.)+
    scale_y_continuous(expand=c(0,0))+
    theme(text=element_text(face='bold'),axis.text.x=element_text(angle=45,hjust=1,size=10),
          axis.title.x=element_blank())+labs(title='Usage',y="percent")


  library(reshape2)


  v_dat<-dcast(v_matrix,type~sample)
  v_dat=v_dat[complete.cases(v_dat),]

  tmp <- v_dat
  rownames(tmp)=tmp[,1]
  tmp=tmp[,-1]
  tmp=t(tmp)
  p3=plot_vdj_usage_ht(tmp)
  ## return v_heatmap

  group_info <- unique(v_matrix[,3:4])
  this_group_list=group_info[match(names(v_dat)[-1],group_info$sample),2]
  tmp <- apply(v_dat, 1, function(x){
    value=as.numeric(x[-1])
    tmp=t.test(value~this_group_list)
    return(c(tmp$estimate,tmp$statistic[1],tmp$p.value))
  })
  tmp <- t(tmp)
  tmp=as.data.frame(tmp)
  colnames(tmp)=c(levels(factor(this_group_list)),'t','p')
  tmp$type=v_dat$type
  tmp$p.adjust=p.adjust(tmp$p)
  stat_t.test=tmp
  ## return stat_t.test

  return(list(p1=p1,p2=p2,p3=p3,dat=v_dat,stat=stat_t.test))

}
