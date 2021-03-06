



#' @rdname colorPCA
#' @title colorPCA
#'
#' @description 
#' Performs principle components analysis on R colors
#' 
#' 
#' @param cols a vector of valid R colors, see ?colors.
#' 
#' 
#' @details 
#' Performs principle components analysis on R colors
#'
#'
#' @examples
#' colorPCA( cols = colors()[as.integer(seq(1,657,length.out = 100))] )
#'
#' @export
#'
colorPCA <- function( cols = NULL ){
  if( is.null(cols) ){
    cols <- grDevices::colors()
  }
  x <- grDevices::col2rgb( cols )
  colnames(x) <- cols
  x2 <- stats::princomp(t(x))
  
  graphics::plot(x2$scores[,1:2], pch=20, col=colnames(x))
  
  return(x2)
}

