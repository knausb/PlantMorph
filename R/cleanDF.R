

#' @rdname cleanDF
#' @title cleanDF
#'
#' @description Sanitizes objects of class data.frame
#' 
#' 
#' @param df an object of class data.frame
#' 
#' 
#' @details 
#' Removes leading and trailing whitespace from row and column names.
#' Replaces consecutive spaces of length greater than one with a single space.
#' 
#' 
cleanDF <- function(df){
  if( !is.null( colnames(df) ) ){
    x <- colnames(df)
    x <- gsub("^\\s{1,}" , "" , x, perl=T)
    x <- gsub( "\\s{1,}$", "" , x, perl=T)
    x <- gsub( "\\s{2,}" , " ", x, perl=T)
    colnames(df) <- x
  }
  
  if( !is.null( rownames(df) ) ){
    x <- rownames(df)
    x <- gsub("^\\s{1,}" , "" , x, perl=T)
    x <- gsub( "\\s{1,}$", "" , x, perl=T)
    x <- gsub( "\\s{2,}" , " ", x, perl=T)
    
    if( length(unique(x)) != nrow(df) ){
      x <- paste(x, 1:length(x), collapse="_")
    }
    
    rownames(df) <- x
  }
  
  clean_col <- function(x){
    if( class(x) == "factor" ){
      FACTOR <- TRUE
      x <- as.character(x)
    }
    x <- gsub("^\\s{1,}" , "" , x, perl=T)
    x <- gsub( "\\s{1,}$", "" , x, perl=T)
    x <- gsub( "\\s{2,}" , " ", x, perl=T)
    if( FACTOR == TRUE ){
      x <- as.factor(x)
    }
  }
  
  df <- apply(df, MARGIN = 2, clean_col )
  
  return(df)
}


