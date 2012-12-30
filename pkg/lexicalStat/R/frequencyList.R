#setGeneric("frequencyList", function(x, ...) {
#  return(standardGeneric("frequencyList"));
#})
#setMethod("frequencyList", signature(x="numeric"), vector2frequencyList);

setClass("FrequencyList",
         contains = "numeric");

############################################################
##
## Constructor
##
############################################################

frequencyList <- function(x) {
  if (!is.numeric(x)) {
      stop("frequencyList needs an numeric vector");
  }
  if (is.null(names(x))) {
      stop("frequencyList needs an named vector");
  }
  if (any("" == names(x))) stop("empty string cannot be a type name");
  class(x) <- "frequencyList";
  obj <- new("FrequencyList", x);
  return(obj);
}

############################################################
##
## Utility functions
##
############################################################

printFrequencyList <- function(x) {
  cat(paste("A frequency list:\n"));
  cat(paste("Number of forms:", length(x), "\n"));
  cat(paste("Number of tokens:", sum(x), "\n"));
  cat(paste("Number of hapax:", sum(x == 1), "\n"));
  n.top.frequency <- min(10, length(x));
  top.frequencies <- sort(x, decreasing=TRUE)[1:n.top.frequency];
  cat(
      paste("Top frequencies :",
	paste(
	  paste(names(top.frequencies), " (", top.frequencies, ")", sep=""),
	  collapse=" "),
	"\n", sep="")
     );
  invisible(x);
}

setMethod("print", signature(x="FrequencyList"), printFrequencyList)

summaryFrequencyList <- function(object){
  print(paste("A FrequencyList with", length(object), "forms and ", sum(objecit), "tokens\n"));
  invisible(object);
}

setMethod("summary", signature(object = "FrequencyList"), summaryFrequencyList)
