#'  basic QC for summary table from igblastn result
#'
#' all reads, alignment reads, reads have cdr3 aa.
#'
#'
#' @param IR_basic_df include columns:v,d,j,cdr3nt,cdr3aa,(type,V_end,V_D_junction,D_region,D_J_junction,J_start)
#' @return basic QC results: all reads, alignment reads, reads have cdr3 aa.
#' @import plyr
#' @import ggplot
#' @import reshape
#' @export
#' @keywords stat_IR_basic
#' @examples
#' #' stat_IR_basic(IR_basic_df)
#'
QC_IR <- function(IR_basic_df){
  all_reads=nrow(IR_basic_df)
  not_qualified_reads=nrow(subset(IR_basic_df,V=='N/A' & cdr3aa == 'N/A'))
  not_cdr3aa = nrow(subset(IR_basic_df, cdr3aa == 'N/A'))
  return(c(all_reads,100*not_qualified_reads/all_reads,100*not_cdr3aa/all_reads))
}
