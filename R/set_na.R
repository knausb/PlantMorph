

#' @rdname set_na
#' @title set_na
#'
#' @description Sets a range of values to NA
#' 
#' 
#' @param x a vector of values within which some values should be set to NA
#' @param na.vector a vector of values that should be changed to NA
#' 
#' 
#' @details 
#' Creates a regex from na.vector and uses this to set values as NA.
#' Values in na.vector are 'wrapped' with '^' and '$' to add specificity.
#' Some characters need to be escaped to form regexs.
#' See \code{\link[base]{regex}} for details.
#' 
#' 
#' @examples
#' myData <- data.frame(matrix(ncol=4, nrow=12))
#' colnames(myData) <- paste('char', 1:ncol(myData), sep="_")
#' rownames(myData) <- paste('sample', 1:nrow(myData), sep="_")
#' set.seed(9)
#' myData[,1] <- abs(round(rnorm(nrow(myData), mean = 5, sd =2)))
#' myData[,2] <- letters[myData[,1]]
#' myData[,3] <- as.factor(myData[,2])
#' myData[,4] <- factor(myData[,2], ordered = TRUE)
#' myData[3,2] <- "?" # Add an nexpected value
#' myData[4,2] <- "-999" # Add an nexpected value
#' myData[,2] <- set_na(myData[,2])
#' myData
#' 
#' 
#' @export
#' 
set_na <- function(x, na.vector = c("\\?", "-999") ){
  myRegex = paste(na.vector, collapse="$|^")
  myRegex <- paste("^", myRegex, "$", sep="")
  
  is.na( x[ grep(myRegex, x) ] ) <- TRUE
  return(x)
}
