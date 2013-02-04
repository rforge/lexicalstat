##############################################################
#' The CorpusAsFrequencies virtual class
#'
#' This virtual class is the root hierarchy of all classes representing corpora with
#' frequency count of linguistic types rather than with the list of actual phenomena.
#'
#' Subclass of \code{CorpusAsFrequencies} must implement the generic methods
#' \code{\link{ntype}} and \code{\link{types}}.
#'
#' @name CorpusAsFrequencies
#' @seealso \code{\link{Corpus}} (for an overview of the available classes) and the subclasses \code{\link{FrequencyList}} and \code{\link{LexicalTable}}
#' @rdname CorpusAsFrequencies
#' @aliases CorpusAsFrequencies-class
#' @exportClass CorpusAsFrequencies
#' @author Sylvain Loiseau
setClass("CorpusAsFrequencies", contains="Corpus");

##############################################################
#' Number of types of a corpus.
#'
#' @param corpus Any \code{\link{CorpusAsFrequencies}} concrete subclass.
#'
#' @return The number of linguistic types (unique tokens) in the corpus.
#' 
#' @seealso \code{\link{types}}
#' @export
#' @docType methods
#' @rdname ntype-methods
#'
#' @examples
#' data(dickensFrequencyList)
#' ntype(dickensFrequencyList);
setGeneric("ntype", function(corpus) standardGeneric("ntype"));

##############################################################
#' List of the types of a corpus.
#'
#' @param corpus Any \code{\link{CorpusAsFrequencies}} concrete subclass.
#'
#' @return A character vector containing the list of the linguistic types (unique tokens) in the corpus.
#' 
#' @seealso \code{\link{ntype}}
#' @export
#' @docType methods
#' @rdname types-methods
#'
#' @examples
#' data(dickensFrequencyList)
#' types(dickensFrequencyList);
setGeneric("types", function(corpus) standardGeneric("types"));

