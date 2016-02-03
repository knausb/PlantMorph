

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
  
  # Clean colnames.
  if( !is.null( colnames(df) ) ){
    x <- colnames(df)
    x <- gsub("^\\s{1,}" , "" , x)
    x <- gsub( "\\s{1,}$", "" , x)
    x <- gsub( "\\s{2,}" , " ", x)
    colnames(df) <- x
  }
  
  # Clean rownames.
  if( !is.null( rownames(df) ) ){
    x <- rownames(df)
    x <- gsub("^\\s{1,}" , "" , x)
    x <- gsub( "\\s{1,}$", "" , x)
    x <- gsub( "\\s{2,}" , " ", x)
    
    if( length(unique(x)) != nrow(df) ){
      x <- paste(x, 1:length(x), collapse="_")
    }
    
    rownames(df) <- x
  }
  
  # Function to clean column values.
  clean_col <- function(x){
    if( class(x) == "factor" ){
      FACTOR <- TRUE
      x <- as.character(x)
    } else {
      FACTOR <- FALSE
    }
    x <- gsub("^\\s{1,}" , "" , x)    
    x <- gsub( "\\s{1,}$", "" , x)
    x <- gsub( "\\s{2,}" , " ", x)
    if( FACTOR == TRUE ){
      x <- as.factor(x)
    }
    return(x)
  }
  
#
  df <- apply(df, MARGIN = 2, clean_col )
  
  return(df)
}


