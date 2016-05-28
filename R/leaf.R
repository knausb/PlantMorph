

#' @rdname leaf
#' @title leaf
#'
#' @description Plot a leaf
#' 
#' 
#' @details 
#' Plot attitude of structures
#' 
#' 
#' @seealso 
#' aplpack::faces
#' stats::density
#' graphics::polygon
#' 
#' 
leaf <- function(){
  leaf <- matrix(ncol=2, nrow=10)
  colnames(leaf) <- c("x", "y")
  leaf[,'y'] <- c(-2:2,2:-2)
  
  leaf[,'x'] <- stats::dnorm( x=leaf[,'y'], mean = 0, sd = 1 )
  leaf[6:10,'x'] <- -1 *   leaf[6:10,'x']
  
  graphics::plot( leaf[,"x"], leaf[,"y"], type="n", xlim=c(-1,1), ylim=c(-2,2))
  graphics::polygon( x = leaf[,"x"], y = leaf[,"y"], col="forestgreen")
}



