#'  statistics for CDR3 diversity
#'
#' In contrast to the previously reported Shannon index and Simpson index,
#' the D50 value is understandable and direct.It is the calculated percentage of dominant unique clones,
#'  accumulative reads of which made up for 50% of the total (ranges from 0 to 50 in theory)
#'
#' @param cdr3aa A vector contain all of the cdr3 sequence
#' @param method which diverity should be cal:shannon/simpson/gini/d50
#' @return a numeric value
#' @export
#' @keywords cdr3_diversity_stat
#' @examples
#' #' cdr3_diversity_stat(cdr3aa,'d50')

cdr3_diversity_stat <- function(cdr3aa,method=c('shannon','simpson','gini','d50')){

  shannon.entropy <-function(y){
    myfreqs <- table(y)/length(y)
    myvec <- as.data.frame(myfreqs)[,2]
    -sum(myvec * log2(myvec))
  }
  Simpson.index <-function(y){
    myfreqs <- table(y)/length(y)
    myvec <- as.data.frame(myfreqs)[,2]
    1-sum( myvec ^2)
  }
  gini.index  <-function(y){
    x<- as.numeric(table(y))
    n <- length(x)
    x <- sort(x)
    G <- sum(x * 1L:n)
    G <- 2 * G/sum(x) - (n + 1L)
    G/n
  }
  d50.index  <-function(y){
    x<- as.numeric(table(y))
    l=length(x)
    state=cumsum(sort(x,decreasing = T))>sum(x)/2
    (l -sum(state))/l
  }
  switch(match.arg(method),
         #'shannon','simpson','gini','d50'
         shannon = shannon.entropy(cdr3aa),
         gini = gini.index(cdr3aa),
         simpson = Simpson.index(cdr3aa),
         d50 = d50.index(cdr3aa)
  )
}
