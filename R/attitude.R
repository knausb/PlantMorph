


#' @rdname attitude
#' @title attitude
#'
#' @description Plot attitude of structures
#' 
#' 
#' @param erect degrees for erect structures.
#' @param ascending degrees for ascending structures.
#' @param spreading degrees for spreading structures.
#' @param reflexed degrees for reflexed structures.
#' 
#' @param ylim the y limits of the plot.
#' @param ... further parameters passed to other methods.
#' 
#' 
#' @details 
#' Plot attitude of structures
#' 
#' @examples
#' attitude()
#' 
attitude <- function( erect = 0, 
                      ascending = 45, 
                      spreading = 90,
                      reflexed = 135,
                      ylim = c(0, 100),
                      ... ){
  plot( x = c(0,0), y = c(0,50), type = "l",
        xlab = "", ylab = "",
        xaxt = "n", yaxt = "n",
        ylim = ylim)
  
#  lines( x = c(0,0), y = c(0,50) )
  
  if( !is.null(erect) ){
    text( x = 0, y = 50, labels = "erect", adj = c(0,0.5), srt = 90-erect )
  }
  
  if( !is.null(ascending) ){
    text( x = 0, y = 45, labels = "ascending", adj = c(0,0.5), srt = 90-ascending )
  }
  
  if( !is.null(spreading) ){
    text( x = 0, y = 40, labels = "spreading", adj = c(0,0.5), srt = 90-spreading )
  }
  
  if( !is.null(reflexed) ){
    text( x = 0, y = 35, labels = "reflexed", adj = c(0,0.5), srt = 90-reflexed )
  }
  
}