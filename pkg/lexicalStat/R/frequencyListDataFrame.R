##############################################################
#' The FrequencyListDataFrame class
#'
#' This class is the default implementation of \code{\link{FrequencyList}}. Considere
#' the function \code{\link{readFrequencyList}} and \code{\link{writeFrequencyList}} for
#' read/write access to files.
#'
#' @name FrequencyListDataFrame
#' @seealso \code{\link{FrequencyList}} ; \code{\link{Corpus}} for an overview of the available classes.
#' @rdname FrequencyListDataFrame
#' @aliases FrequencyListDataFrame-class
#' @exportClass FrequencyListDataFrame
#' @author Sylvain Loiseau
setClass("FrequencyListDataFrame",
         contains = c("data.frame", "FrequencyList"));

############################################################
##
## Implementation of Corpus
##
############################################################

##############################################################
#' @rdname N-methods
#' @aliases N,FrequencyListDataFrame-method
setMethod("N", "FrequencyListDataFrame", function(corpus) sum(corpus$frequency));

############################################################
##
## Implementation of CorpusAsFrequencies
##
############################################################

##############################################################
#' @rdname ntype-methods
#' @aliases ntype,FrequencyListDataFrame-method
setMethod("ntype", "FrequencyListDataFrame", function(corpus) nrow(corpus));

##############################################################
#' @rdname types-methods
#' @aliases types,FrequencyListDataFrame-method
setMethod("types", "FrequencyListDataFrame", function(corpus) sort(as.character(corpus$type)));

############################################################
##
## Implementation of FrequencyList
##
############################################################

##############################################################
#' @rdname freq-methods
#' @aliases freq,FrequencyListDataFrame,character-method
setMethod("freq", c("FrequencyListDataFrame", "character"), function(corpus, types) {
  f <- corpus[ match(types, corpus[,1]), 2 ];
  names(f) <- types;
  return(f);
});

##############################################################
#' @rdname contains.types-methods
#' @aliases contains.types,FrequencyListDataFrame,character-method
setMethod("contains.types", c("FrequencyListDataFrame", "character"), function(corpus, types) {
  exists <- types %in% corpus[,1]
  names(exists) <- types;
  return(exists);
});

##############################################################
#' @rdname hapax-methods
#' @aliases hapax,FrequencyListDataFrame-method
setMethod("hapax", "FrequencyListDataFrame", function(corpus) {
  corpus[corpus[,2]==1,1];
});

############################################################
##
## Utility functions
##
############################################################

setMethod("summary", signature(object = "FrequencyListDataFrame"), function(object){
  cat(paste("A frequency list:\n"));
  cat(paste("Number of types:", ntype(object), "\n"));
  cat(paste("Number of tokens:", N(object), "\n"));
  cat(paste("Number of hapax:", length(hapax(object)), "\n"));
  n.top.frequency <- min(10, length(object));
  top.frequencies <- order(object$frequency, decreasing=TRUE)[1:n.top.frequency];
  cat(
      paste("Top frequencies :",
	paste(
	  paste(object$type[top.frequencies], " (", object$frequency[top.frequencies], ")", sep=""),
	  collapse=" "),
	"\n", sep="")
     );
  invisible(object);
});

############################################################
##
## Read/write
##
############################################################

readFrequencyList <- function(filename, header=FALSE, sep="\t") {
  x <- read.table(filename, header=header, colClasses=c("character", "numeric"), sep="\t");
  y <- as.vector(x[,2]);
  names(y) <- x[,1];
  return(frequencyList(y));
}

writeFrequencyList <- function(frequencyList, filename) {
  if (class(frequencyList != "FrequencyListDataFrame")) stop("must be a 'FrequencyListDataFrame' object");
  df <- data.frame(names(frequencyList), frequencyList);
  x <- write.table(df, file=filename, header=FALSE, colClasses=c("character", "numeric"), sep="\t");
}


