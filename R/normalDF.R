


#' @rdname normalDF
#' @title normalDF
#'
#' @description Tests columns of objects of class data.frame for normality
#' 
#' 
#' @param df an object of class data.frame
#' 
#' 
#' @details 
#' Tests columns (characters) for normality.
#' If columns are factors they are converted to characters.
#' If columns are characters they are converted to numerics.
#' 
#' @seealso 
#' \code{\link[stats]{shapiro.test}}
#' 
normalDF <- function(df){
  
  report <- data.frame( matrix( ncol=5, nrow=ncol(df) ) )
  colnames(report) <- c("Character", "Class", "Percent NA",
                        "SW stat", "p-value")
  
  report[,1] <- colnames(df)
#  report[,2] <- apply(df, MARGIN=2, class)
  report[,3] <- apply(df, MARGIN=2, function(x){ sum( is.na(x) )/length(x) })
  
  for( i in 1:ncol(df) ){
    x <- df[,i]
    report[i,2] <- class(x)
    if( class(x) == "factor" ){
      x <- as.numeric(x)
    }
#    if( class(x) == "character" ){
#      x <- as.numeric(x)
#    }
    x <- stats::shapiro.test(x)
    report[i,4] <- x$statistic
    report[i,5] <- x$p.value
  }
  
  return(report)
}



