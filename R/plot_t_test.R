#' boxplot for t.test result of two group of data
#'
#' boxplot and jitter plot
#'
#'
#' @param a value vector for first  group in numeric
#' @param b value vector for second group in numeric
#' @param file_out the filename of output figue,should end up with pdf or png
#' @return NULL
#' @import ggplot2
#' @import RColorBrewer
#' @export
#' @keywords plot_t_test
#' @examples
#' #' plot_t_test(stat_matrix,'cdr3aa-distribution-stat.pdf')


## t test for the CDR3 diversity
plot_t_test <- function(a,b,file_out){
  my.plot <- function(...) {
    Ttest <- t.test(a,b)
    dat2 <- data.frame(values = c(a,b),
                       type = c(rep('case',length(a)),rep('control',length(b)))
    )
    mainText=paste0("T test","\n","P.value=", sprintf("%04f",Ttest$p.value),sep='')
    boxplot( values ~  type, data = dat2, lwd = 2, ylab = 'value',las=1,main=mainText)
    stripchart(values ~ type, vertical = TRUE, data = dat2,
               method = "jitter", add = TRUE, pch = 20, col = 'blue')

  }
  custom.dev <- function(fname) {
    if (grepl("\\.pdf$",fname)){
      pdf(fname)
    } else if (grepl("\\.png$",fname)) {
      png(fname, res=120)
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
  }

}


