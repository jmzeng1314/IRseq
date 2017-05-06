#' Circle plot for V and J usage
#'
#' It will generate a published quality figure according to the V and J usage marix.
#'
#' @param vj_usage_matrix columns and rows correspond to V and J segments respectively
#' @param file_out the filename of output figue,should end up with pdf or png
#' @return NULL
#' @import circlize
#' @import RColorBrewer
#' @export
#' @keywords circle
#' @examples
#' #' v_j_circle(vj_usage_matrix,'v-j-circle.pdf')

v_j_circle <- function(vj_usage_matrix,file_out){
  require(circlize); require(RColorBrewer)

  rn = rownames(vj_usage_matrix)
  cn = colnames(vj_usage_matrix)
  mat <-vj_usage_matrix* 100
  n <- nrow(vj_usage_matrix)
  m <- ncol(vj_usage_matrix)

  # Here columns and rows correspond to V and J segments respectively
  # sort

  col_sum = apply(mat, 2, sum)
  row_sum = apply(mat, 1, sum)

  mat <- mat[order(row_sum), order(col_sum)]

  # equal number of characters for visualizaiton

  rn <- rownames(mat)
  cn <- colnames(mat)

  maxrn <- max(nchar(rn))
  maxcn <- max(nchar(cn))

  for(i in seq_len(length(rn))) {
    rn[i] <- paste(rn[i], paste(rep(" ", maxrn - nchar(rn[i])), collapse = ''))
  }

  for(i in seq_len(length(cn))) {
    cn[i] <- paste(cn[i], paste(rep(" ", maxcn - nchar(cn[i])), collapse = ''))
  }

  rownames(mat) <- rn
  colnames(mat) <- cn

  # plotting function for simplicity
  my.plot <- function(...) {
    # viz using circlize
    circos.par(gap.degree = c(rep(1, nrow(mat)-1), 10, rep(1, ncol(mat)-1), 15), start.degree = 5)

    rcols <- rep(brewer.pal(12, "Paired"), nrow(mat)/12 + 1)[1:nrow(mat)]
    ccols <- rep(brewer.pal(12, "Paired"), ncol(mat)/12 + 1)[1:ncol(mat)]

    names(rcols) <- sort(rownames(mat))
    names(ccols) <- sort(colnames(mat))

    chordDiagram(mat, annotationTrack = "grid",
                 grid.col = c(rcols, ccols),
                 preAllocateTracks = list(track.height = 0.2), transparency = 0.5)

    circos.trackPlotRegion(track.index = 1, bg.border = NA,
                           panel.fun = function(x, y) {
                             sector.name = get.cell.meta.data("sector.index")
                             xlim = get.cell.meta.data("xlim")
                             ylim = get.cell.meta.data("ylim")
                             circos.text(mean(xlim), ylim[1], cex = 0.5, sector.name, facing = "clockwise", adj = c(0, 0.5))
                           }
    )

    circos.clear()

  }

  custom.dev <- function(fname) {
    if (grepl("\\.pdf$",fname)){
      pdf(fname)
    } else if (grepl("\\.png$",fname)) {
      png(fname, width     = 3.25,
          height    = 3.25,
          units     = "in",
          res       = 1200,
          pointsize = 4)
    } else {
      stop('Unknown plotting format')
    }
  }

  custom.dev(file_out)

  my.plot()

  dev.off()


}
