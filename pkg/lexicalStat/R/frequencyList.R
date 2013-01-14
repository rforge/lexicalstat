setClass("FrequencyList",
         contains = "data.frame");

############################################################
##
## Constructor
##
############################################################

frequencyList <- function (x) UseMethod("frequencyList");

frequencyList.numeric <- function(x) {
  if (is.null(names(x))) {
    stop("frequencyList needs an named vector");
  }
  y <- data.frame(type=names(x), frequency=as.numeric(x), stringsAsFactors=FALSE);
  frequencyList(y);
}

frequencyList.table <- function(x) {
    n <- names(x);
    y <- as.numeric(x);
    names(y) <- n;
    frequencyList(y);
}

frequencyList.data.frame <- function(x) {
  if (ncol(x) != 2) stop("x must have two columns ('type' and 'frequency')");
  if (!is.character(x[,1])) stop("x$type must be a character vector");
  if (any(x[,1] == "")) stop("a types cannot be an empty string");
  if (!is.numeric(x[,2])) stop("x$frequency must be numeric");
  if (!all(names(x) == c("type", "frequency"))) stop("the data.frame must have column names c('type', 'frequency')");
  obj <- new("FrequencyList", x);
  return(obj);
}

############################################################
##
## Functions specific to this class (see also corpus.R for accessors common to other Corpus classes)
##
############################################################

setGeneric("freq", function(obj, types) standardGeneric("freq"));

setMethod("freq", c("FrequencyList", "character"), function(obj, types) {
  f <- obj[
      match(types, obj[,1]),
      2
    ]
  return(f);
});

setGeneric("has.types", function(obj, types) standardGeneric("has.types"));

setMethod("has.types", c("FrequencyList", "character"), function(obj, types) {
  types %in% obj[,1]
});

setGeneric("hapax", function(obj) {
  return(standardGeneric("hapax"));
})

setMethod("hapax", "FrequencyList", function(obj) names(obj)[obj == 1] )

############################################################
##
## Utility functions
##
############################################################

#setMethod("show", signature(object="FrequencyList"), function(object) {
#  print(object);
#});
#
#setMethod("print", signature(x="FrequencyList"), function(x) {
#
#});

setMethod("summary", signature(object = "FrequencyList"), function(object){
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
  if (class(frequencyList != "FrequencyList")) stop("must be a frequencyList object");
  df <- data.frame(names(frequencyList), frequencyList);
  x <- write.table(df, file=filename, header=FALSE, colClasses=c("character", "numeric"), sep="\t");
}


