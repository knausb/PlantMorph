

#' @rdname character_report
#' @title character_report
#'
#' @description Prepare a report for characters
#' 
#' 
#' @param chars a data.frame containing samples (rows) and characters (columns).
#' @param file a character string giving the output file name.
#' @param meta logical specifying whether the meta data should be printed.
#' 
#' @details 
#' Prepare an RMarkdown report for characters.
#' 
#' 
#' @examples 
#' data("iris")
#' character_report( iris[,-5] )
#' 
#' 
character_report <- function( chars ,
                              file = "character_report.Rmd",
                              meta = TRUE ){
  
  if( file != "" ){
    unlink( file )
  }

  # Meta region.
  if( meta == TRUE){
    cat("---", file = file, sep='\n', append = TRUE)
    cat('title: "Character Report"', file = file, sep='\n', append = TRUE)
    cat('output:', file = file, sep='\n', append = TRUE)
    cat('  html_document:', file = file, sep='\n', append = TRUE)
    cat('    toc: true', file = file, sep='\n', append = TRUE)
    cat('    theme: united', file = file, sep='\n', append = TRUE)
    cat("---", file = file, sep='\n', append = TRUE)
    cat("", file = file, sep='\n', append = TRUE)
    cat("", file = file, sep='\n', append = TRUE)
  }
  
  
  # Global knitr options
  cat('```{r global_options, include=FALSE}', file = file, sep='\n', append = TRUE)
  cat('knitr::opts_chunk$set(fig.align="center", fig.width=6,
                        fig.height=4, echo=FALSE)',
      file = file, sep='\n', append = TRUE)
  cat('```' , file = file, sep='\n', append = TRUE)
  cat("", file = file, sep='\n', append = TRUE)
  cat("", file = file, sep='\n', append = TRUE)
  
  
  # Data structure
  cat('## Data structure', file = file, sep='\n', append = TRUE)
  cat("Code suppressed.", file = file, sep='\n', append = TRUE)
  cat("", file = file, sep='\n', append = TRUE)
  cat('```{r}' , file = file, sep='\n', append = TRUE)
  dump( "chars", file = file, append = TRUE )
  cat('```' , file = file, sep='\n', append = TRUE)
  cat("", file = file, sep='\n', append = TRUE)
  cat("", file = file, sep='\n', append = TRUE)
  
  
  # Characters
  
#  require(RColorBrewer)
  mypalette <- rep( RColorBrewer::brewer.pal( n=9, name="Pastel1"), times=ncol(chars) )

  for( i in 1:ncol(chars) ){
    char_name <- colnames(chars)[i]
    
    cat(paste('##', char_name), file = file, sep='\n', append = TRUE)
    cat("", file = file, sep='\n', append = TRUE)
    
    # R code chunk
    cat('```{r}' , file = file, sep='\n', append = TRUE)
    cat( paste('hist( chars[,', i, '], main = "', char_name, '", col = "', mypalette[i], '", xlab="" )', sep=""),
         file = file, sep='\n', append = TRUE)
    cat( paste('st <- shapiro.test( chars[,', i, '] )' , sep=""), file = file, sep='\n', append = TRUE )
    cat('```' , file = file, sep='\n', append = TRUE)
    
    cat("", file = file, sep='\n', append = TRUE)
    
    cat('Shapiro test for normality resulted in a statistic of `r st$statistic` (p-value: `r st$p.value`).' , file = file, sep='\n', append = TRUE)
    
    # R code chunk
    cat('```{r}' , file = file, sep='\n', append = TRUE)
    cat('if( st$p.value <= 0.05){' , file = file, sep='\n', append = TRUE)
    cat('message("Assumption of normality is rejected.")' , file = file, sep='\n', append = TRUE)
    cat('} else {' , file = file, sep='\n', append = TRUE)
    cat('message("Assumption of normality is not rejected.")' , file = file, sep='\n', append = TRUE)
    cat('}' , file = file, sep='\n', append = TRUE)
    cat('```' , file = file, sep='\n', append = TRUE)

        
    cat("", file = file, sep='\n', append = TRUE)
    cat("", file = file, sep='\n', append = TRUE)
  }

}