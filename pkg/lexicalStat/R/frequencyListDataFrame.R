setClass("FrequencyListDataFrame",
         contains = c("data.frame", "FrequencyList"));

############################################################
##
## Implementation of Corpus
##
############################################################

setMethod("N", "FrequencyListDataFrame", function(obj) sum(obj$frequency));

setMethod("ntype", "FrequencyListDataFrame", function(obj) nrow(obj));

setMethod("types", "FrequencyListDataFrame", function(obj) sort(as.character(obj$type)));

############################################################
##
## Implementation of FrequencyList
##
############################################################

setMethod("freq", c("FrequencyListDataFrame", "character"), function(obj, types) {
  f <- obj[ match(types, obj[,1]), 2 ];
  return(f);
});

setMethod("contains.types", c("FrequencyListDataFrame", "character"), function(obj, types) {
  types %in% obj[,1]
});

setMethod("hapax", "FrequencyListDataFrame", function(obj) names(obj)[obj == 1] )

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


