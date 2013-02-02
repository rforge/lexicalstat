##############################################################
#' The FrequencyList virtual class
#'
#' This virtual class is a subclass of \code{\link{CorpusAsFrequencies}} and the root
#' hierarchy of all classes representing corpora as a list of types with their total frequencies.
#'
#' Subclass of \code{FrequencyList} must implement the generic methods
#' \code{\link{freq}}, \code{\link{hapax}} and \code{\link{contains.types}}.
#'
#' @name FrequencyList
#' @seealso its superclass \code{\link{CorpusAsFrequencies}}; its sibling \code{\link{LexicalTable}}; one of its implementation: \code{\link{FrequencyListDataFrame}}
#' @rdname FrequencyList
#' @aliases FrequencyList-class
#' @exportClass FrequencyList
#' @author Sylvain Loiseau
setClass("FrequencyList", contains="CorpusAsFrequencies");

##############################################################
#' Get the frequencies of given linguistic type.
#'
#' @param corpus Any \code{\link{FrequencyList}} concrete subclass.
#'
#' @param types The linguistic types for which the frequency is requested.
#'
#' @return A named numeric vector of frequencies.
#' 
#' @export
#' @docType methods
#' @rdname freq-methods
#'
#' @examples
#' data(dickensFrequencyList)
#' freq(dickensFrequencyList);
setGeneric("freq", function(corpus, types) {
  standardGeneric("freq")
});

##############################################################
#' Ask if a FrequencyList contains the given linguistic types.
#'
#' @param corpus Any \code{\link{FrequencyList}} concrete subclass.
#'
#' @param types The linguistic types.
#'
#' @return A named logical vector.
#' 
#' @export
#' @docType methods
#' @rdname contains.types-methods
#'
#' @examples
#' data(dickensFrequencyList)
#' contains.types(dickensFrequencyList, c("the", "xxxx"));
setGeneric("contains.types", function(corpus, types) {
  standardGeneric("contains.types")
});

##############################################################
#' The list of linguistic types occurring only once.
#'
#' @param corpus Any \code{\link{FrequencyList}} concrete subclass.
#'
#' @return A character vector.
#' 
#' @export
#' @docType methods
#' @rdname hapax-methods
#'
#' @examples
#' data(dickensFrequencyList)
#' hapax(dickensFrequencyList);
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
