


#' @rdname character_report2
#' @title character_report2
#'
#' @description Create a RMarkdown report about character classes and distribution
#' 
#' 
#' @param x a data.frame containing samples (rows) and characters (columns).
#' 
#' @details 
#' Prepare an RMarkdown report for characters.
#' 
#' 
#' @examples 
#' myData <- data.frame(matrix(1:8, ncol=4, nrow=8))
#' colnames(myData) <- paste('char', 1:ncol(myData), sep="_")
#' rownames(myData) <- paste('sample', 1:nrow(myData), sep="_")
#' myData[,2] <- as.character(myData[,2])
#' myData[,3] <- as.factor(myData[,3])
#' myData[,4] <- factor(myData[,4], ordered = TRUE)
#' 
#' \dontrun{
#' character_report2(myData)
#' }
#' 
#' @export
#' 
character_report2 <- function(x){
  save(x, file="character_report.RData")
  write('---', file='character_report.Rmd')
  write('title: "Character Report"', file='character_report.Rmd', append = TRUE)
  write('author: "Brian J. Knaus"', file='character_report.Rmd', append = TRUE)
  write('date: "`r Sys.Date()`"', file='character_report.Rmd', append = TRUE)
  write('output:', file='character_report.Rmd', append = TRUE)
  write('  html_document:', file='character_report.Rmd', append = TRUE)
  write('    toc: true', file='character_report.Rmd', append = TRUE)
  write('---', file='character_report.Rmd', append = TRUE)
  write('', file='character_report.Rmd', append = TRUE)
  write('```{r setup, include=FALSE}', file='character_report.Rmd', append = TRUE)
  write('knitr::opts_chunk$set(fig.align = "center")', file='character_report.Rmd', append = TRUE)
  write('```', file='character_report.Rmd', append = TRUE)
  write('', file='character_report.Rmd', append = TRUE)
  write('```{r load data, echo=FALSE}', file='character_report.Rmd', append = TRUE)
  write('load(file="character_report.RData")',
        file='character_report.Rmd', append = TRUE)
  #  dump('x', file='character_report.Rmd', append = TRUE)
  write('```', file='character_report.Rmd', append = TRUE)
  for(i in 1:ncol(x)){
    write('', file='character_report.Rmd', append = TRUE)
    write('*****', file='character_report.Rmd', append = TRUE)
    write('', file='character_report.Rmd', append = TRUE)
    #    write( paste('## `r Character ', i, ': ', colnames(x)[i], '`', sep=""),
    #    write( paste('## Character ', i, ' ', colnames(x)[i], '`', sep=""),
    write( paste('## Character ', i, ' ', colnames(x)[i], sep=""),
           file='character_report.Rmd', append = TRUE)
    write('', file='character_report.Rmd', append = TRUE)
    #    write('```{r, results="asis"}', file='character_report.Rmd', append = TRUE)
    write('```{r, results="asis", echo = FALSE}', file='character_report.Rmd', append = TRUE)
    write( paste('if( is.ordered( x[ ,colnames(x)[', i, '] ] ) ){', sep=""), file='character_report.Rmd', append = TRUE)
    #    write( paste('  tmp <- levels(x[ ,colnames(x)[', i, '] ])', sep=""), file='character_report.Rmd', append = TRUE)
    write( paste('  tmp <- as.character( x[ ,colnames(x)[', i, '] ] )', sep=""), file='character_report.Rmd', append = TRUE)
    write( '  tbl.title <- "Ordered factor" ', file='character_report.Rmd', append = TRUE)
    write('} else {', file='character_report.Rmd', append = TRUE)
    write( paste('  tmp <- x[ ,colnames(x)[', i, '] ]', sep=""), file='character_report.Rmd', append = TRUE)
    write('  tmp <- as.character(tmp)', file='character_report.Rmd', append = TRUE)
    #    write('  tmp <- unique(tmp)', file='character_report.Rmd', append = TRUE)
    write('  tmp <- na.omit(tmp)', file='character_report.Rmd', append = TRUE)
    write( '  tbl.title <- "Character" ', file='character_report.Rmd', append = TRUE)
    write('}', file='character_report.Rmd', append = TRUE)
    write('', file='character_report.Rmd', append = TRUE)
    write('tmp2 <- suppressWarnings( as.numeric(tmp) )', file='character_report.Rmd', append = TRUE)
    write('numbs <- tmp2[ !is.na( tmp2 ) ]', file='character_report.Rmd', append = TRUE)
    write('chars <- tmp[ is.na( tmp2 ) ]', file='character_report.Rmd', append = TRUE)
    write('chars.m <- as.matrix( table( chars ) )', file='character_report.Rmd', append = TRUE)
    #    write('chars.m <- as.matrix(chars)', file='character_report.Rmd', append = TRUE)
    #    write('colnames(chars.m) <- "Character table"', file='character_report.Rmd', append = TRUE)
    write('colnames(chars.m) <- tbl.title', file='character_report.Rmd', append = TRUE)
    write('', file='character_report.Rmd', append = TRUE)
    write('if( length(numbs) >= 1 ){', file='character_report.Rmd', append = TRUE)
    write('  # Appears numeric.', file='character_report.Rmd', append = TRUE)
    write( paste('  hist( numbs, col=', i, ', main=colnames(x)[', i, '], xlab="")', sep=""), file='character_report.Rmd', append = TRUE)
    write('}', file='character_report.Rmd', append = TRUE)
    write('', file='character_report.Rmd', append = TRUE)
    write('if( nrow(chars.m) > 0 ){', file='character_report.Rmd', append = TRUE)
    write('  if( nrow(chars.m) <= 10 ){', file='character_report.Rmd', append = TRUE)
    write('    knitr::kable(chars.m, caption = "All characters")', file='character_report.Rmd', append = TRUE)
    write('  } else {', file='character_report.Rmd', append = TRUE)
    #    write('    chars.m[10, 1] <- "Only 10"', file='character_report.Rmd', append = TRUE)
    write('    knitr::kable(chars.m[1:10, , drop = FALSE ], caption = "First 10 characters.")', file='character_report.Rmd', append = TRUE)
    write('', file='character_report.Rmd', append = TRUE)
    #    write('    message("Only first 10 rows presented")', file='character_report.Rmd', append = TRUE)
    write('  }', file='character_report.Rmd', append = TRUE)
    write('}', file='character_report.Rmd', append = TRUE)
    write('```', file='character_report.Rmd', append = TRUE)
    write('', file='character_report.Rmd', append = TRUE)
  }
}


