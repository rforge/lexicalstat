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
  
  if (class(x) == "table") {
    n <- names(x);
    x <- as.numeric(x);
    names(x) <- n;
  }
  if (any("" == names(x))) stop("empty string cannot be a type name");
  obj <- new("FrequencyList", x);
  return(obj);
}

############################################################
##
## Utility functions
##
############################################################

setMethod("print", signature(x="FrequencyList"), function(x) {
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
});

setMethod("summary", signature(object = "FrequencyList"), function(object){
  print(paste("A FrequencyList with", length(object), "forms and ", sum(objecit), "tokens\n"));
  invisible(object);
});
