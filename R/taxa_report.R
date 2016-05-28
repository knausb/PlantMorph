

#' @rdname taxa_report
#' @title taxa_report
#'
#' @description Prepare a report for characters as a function of taxa
#' 
#' 
#' @param chars a data.frame containing samples (rows) and characters (columns).
#' @param taxa a data.frame containing samples (rows) and a column of class factor named 'taxa' (groupings).
#' @param file a character string giving the output file name.
#' @param meta logical specifying whether the meta data should be printed.
#' 
#' @details 
#' Prepare an RMarkdown report for characters.
#' 
#' 
#' @examples 
#' data("iris")
#' rownames(iris) <- paste(iris$Species, 1:nrow(iris), sep="")
#' colnames(iris)[5] <- "taxa"
#' taxa_report( chars = iris[,-5], taxa = iris[ ,5, drop=FALSE] )
#' 
#' @export
#' 
taxa_report <- function( chars,
                         taxa,
                         file = "taxa_report.Rmd",
                         meta = TRUE ){
  
  # Sanity checks
  if( class(chars) != "data.frame" ){
    stop("object chars is not of class data.frame!")
  }
  if( class(taxa) != "data.frame" ){
    stop("object taxa is not of class data.frame!")
  }
  if( sum(rownames(chars) == rownames(taxa)) != nrow(chars) ){
    stop("rownames for chars and taxa do not match!")
  }
  if( sum(rownames(chars) == 1:nrow(chars)) == nrow(chars) ){
    warning("rownames for chars are sequential and assume the same order in taxa.")
  }

  # Clobber file.
  if( file != "" ){
    unlink( file )
  }
  
  # Meta region.
  if( meta == TRUE){
    cat("---", file = file, sep='\n', append = TRUE)
    cat('title: "Categorical Report"', file = file, sep='\n', append = TRUE)
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
  cat('knitr::opts_chunk$set(fig.align="center", fig.width=10,
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
  dump( "taxa", file = file, append = TRUE )
  cat('```' , file = file, sep='\n', append = TRUE)
  cat("", file = file, sep='\n', append = TRUE)
  cat("", file = file, sep='\n', append = TRUE)
  
  # Sanity check of row names
  cat('```{r}' , file = file, sep='\n', append = TRUE)
  cat('if( class(chars) != "data.frame" ){' , file = file, sep='\n', append = TRUE)
  cat('stop("object chars is not of class data.frame!")' , file = file, sep='\n', append = TRUE)
  cat('}' , file = file, sep='\n', append = TRUE)
  cat('if( class(taxa) != "data.frame" ){' , file = file, sep='\n', append = TRUE)
  cat('stop("object taxa is not of class data.frame!")' , file = file, sep='\n', append = TRUE)
  cat('}' , file = file, sep='\n', append = TRUE)
  
  cat('if( sum(rownames(chars) == rownames(taxa)) != nrow(chars) ){' , file = file, sep='\n', append = TRUE)
  cat('stop("rownames for chars and taxa do not match!")' , file = file, sep='\n', append = TRUE)
  cat('}' , file = file, sep='\n', append = TRUE)
  cat('if( sum(rownames(chars) == 1:nrow(chars)) == nrow(chars) ){' , file = file, sep='\n', append = TRUE)
#  cat('if( rownames(chars) == as.character( 1:nrow(chars) ) ){' , file = file, sep='\n', append = TRUE)
  cat('warning("rownames for chars are sequential and assume the same order in taxa.")' , file = file, sep='\n', append = TRUE)
  cat('}' , file = file, sep='\n', append = TRUE)
  cat('```' , file = file, sep='\n', append = TRUE)
  cat("", file = file, sep='\n', append = TRUE)
  cat("", file = file, sep='\n', append = TRUE)
  
  # Create a palette
  mypalette <- rep( RColorBrewer::brewer.pal( n=9, name="Pastel1"), times=ncol(chars) )
  
  
  for( i in 1:ncol(chars) ){
    char_name <- colnames(chars)[i]
    
    cat(paste('##', char_name), file = file, sep='\n', append = TRUE)
    cat("", file = file, sep='\n', append = TRUE)
    
    # R code chunk
    cat('```{r}' , file = file, sep='\n', append = TRUE)
    cat( paste('x <- data.frame( taxa = taxa[,"taxa"], char = chars[,', i, '] )'), file = file, sep='\n', append = TRUE)
#    cat( 'boxplot(x[,2] ~ x[,1])', file = file, sep='\n', append = TRUE)
    cat( 'require(ggplot2)', file = file, sep='\n', append = TRUE)
    cat( 'p <- ggplot2::ggplot(x, aes(x=taxa, y=char, fill=taxa))', file = file, sep='\n', append = TRUE)
    cat( 'p <- p + geom_violin(adjust=1.0, scale = "count", trim=TRUE)', file = file, sep='\n', append = TRUE)
    cat( 'p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))', file = file, sep='\n', append = TRUE)
    cat( paste('p <- p + ylab("', char_name, '")', sep=''), file = file, sep='\n', append = TRUE)
    cat( 'p', file = file, sep='\n', append = TRUE)
    
    # Missingness table
    cat( 'miss <- as.data.frame(table(x$taxa))' , file = file, sep='\n', append = TRUE)
    cat( 'miss$miss <- NA' , file = file, sep='\n', append = TRUE)
    cat( 'for(i in 1:nrow(miss)){' , file = file, sep='\n', append = TRUE)
    cat( '  tmp <- x[x[,1] == miss[i,1],]' , file = file, sep='\n', append = TRUE)
    cat( '  miss[i,3] <- sum(is.na(tmp[,2]))' , file = file, sep='\n', append = TRUE)
    cat( '}' , file = file, sep='\n', append = TRUE)
    cat( 'colnames(miss) <- c("Taxon","Count","NAs")' , file = file, sep='\n', append = TRUE)
    cat( 'knitr::kable(miss)' , file = file, sep='\n', append = TRUE)
    
    
    cat('```' , file = file, sep='\n', append = TRUE)
    
    cat("", file = file, sep='\n', append = TRUE)
    
  }
}

