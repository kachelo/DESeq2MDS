#' PlotMDS for a DESeq2 object
#'
#' @description This function takes a DESeqTRansform object and performs MDS transformation on it to plot it
#' @author Cristóbal Fresno
#' @param rld A \code{\link{DESeqTransform}} object from DESeq2 library
#' @export
#' @import DESeq2
#' @importFrom ggplot2 ggplot aes geom_point labs
#' @examples
#' library("DESeq2")
#' library("ggplot2")
#' library("cowplot")
#' dds <- makeExampleDESeqDataSet(betaSD=1)
#' rld <- rlog(dds)
#' plotMDS(rld)
#'

plotMDS <- function(rld){
  d <- dist( t( assay(rld) ) )
  mdsResult <- cmdscale( d )
  mdsResult <- as.data.frame( mdsResult )
  colnames( mdsResult ) <- sprintf( "coord%d", seq_len(ncol(mdsResult)))
  mdsResult$condition <- colData(rld)$condition
  pl <- ggplot( mdsResult, aes(coord1, coord2, col=condition)) +
    geom_point() + labs(x="Coordinate 1", y="Coordinate 2")
  pl
}
