#' For package dependecies only
#'
#' So far, I just find the way to install the packages in CRAN, not bioconductor.
#' It will automatelly download a list of packges and load them in current R environment.
#'
#' @param pkgs A list of packages
#' @return NULL
#' @export
#' @keywords package
#' @examples
#' #' pkgs_install(c('FField','RColorBrewer','VennDiagram','ape'))


pkgs_install <- function(pkgs=c('FField','RColorBrewer','VennDiagram','ape')){

  #list.of.packages <- c("xx", "yy") # replace xx and yy with package names
  new.packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(new.packages, require, character.only=T)

}
