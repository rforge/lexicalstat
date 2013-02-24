##############################################################
setClass("FrequencyListDataFrame",
         contains = c("data.frame", "FrequencyList"));

############################################################
##
## Implementation of Corpus
##
############################################################

##############################################################
setMethod("N", "FrequencyListDataFrame", function(corpus) sum(corpus$frequency));

############################################################
##
## Implementation of CorpusAsFrequencies
##
############################################################

##############################################################
setMethod("ntype", "FrequencyListDataFrame", function(corpus) nrow(corpus));

##############################################################
setMethod("types", "FrequencyListDataFrame", function(corpus) sort(as.character(corpus$type)));

############################################################
##
## Implementation of FrequencyList
##
############################################################

##############################################################
setMethod("freq", c("FrequencyListDataFrame", "character"), function(corpus, types) {
  .arg_notEmpty(types);
  index <- match(types, corpus[,1]);
  if (any(is.na(index))) {
    stop(paste(
        "some 'types' does not exist in this frequency list: ",
        paste(types[is.na(index)], collpase=" ")
    ));
  }
  f <- corpus[ index, 2 ];
  names(f) <- types;
  return(f);
});

##############################################################
setMethod("contains.types", c("FrequencyListDataFrame", "character"), function(corpus, types) {
  .arg_notEmpty(types);
  exists <- types %in% corpus[,1]
  names(exists) <- types;
  return(exists);
});

##############################################################
setMethod("hapax", "FrequencyListDataFrame", function(corpus) {
  return(corpus[corpus[,2]==1,1]);
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
  x <- write.table(df, file=filename, col.names=TRUE, sep="\t");
}


