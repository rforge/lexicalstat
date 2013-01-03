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
  y <- data.frame(type=as.factor(names(x)), frequency=as.numeric(x));
  frequencyList(y);
}
  
frequencyList.table <- function(x) {
    n <- names(x);
    y <- as.numeric(x);
    names(y) <- n;
    frequencyList(y);
}

frequencyList.data.frame <- function(x) {
  msg <- "a frequency list can be built with a data.frame of two columns named 'type' and 'frequency'";
  if (ncol(x) != 2) stop("x must have two columns ('type' and 'frequency')");
  if (!is.factor(x[,1])) stop("x$type must be a factor");
  if (!is.numeric(x[,2])) stop("x$frequency must be numeric");
  if (!all(names(x) == c("type", "frequency"))) stop(msg);
  obj <- new("FrequencyList", x);
  return(obj);
}

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
  cat(paste("Number of types:", V(object), "\n"));
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


