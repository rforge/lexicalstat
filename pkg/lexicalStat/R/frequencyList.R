setClass("FrequencyList", contains="CorpusAsFrequencies");

############################################################
##
##
## freq
##
##
############################################################

setGeneric("freq", function(obj, types) {
  standardGeneric("freq")
});

############################################################
##
##
## contains.types
##
##
############################################################

setGeneric("contains.types", function(obj, types) {
  standardGeneric("contains.types")
});

############################################################
##
##
## hapax
##
##
############################################################

setGeneric("hapax", function(obj) {
  return(standardGeneric("hapax"));
})

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
  obj <- new("FrequencyListDataFrame", x);
  return(obj);
}
