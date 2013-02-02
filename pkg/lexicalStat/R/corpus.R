##############################################################
#' The Corpus class
#'
#' This virtual class is the root hierarchy of all classes available for representing
#' linguistic corpora.
#'
#' Subclass of \code{Corpus} must implement the generic methods \code{\link{N}}.
#'
#' @name Corpus
#' @rdname Corpus
#' @seealso The direct virtual subclasses \code{\link{CorpusAsFrequencies}} and \code{\link{CorpusAsTokens}}
#' @aliases Corpus-class
#' @exportClass Corpus
#' @author Sylvain Loiseau
setClass("Corpus");

##############################################################
#' Number of tokens of a corpus.
#'
#' @param corpus Any \code{\link{corpus}} concrete subclass.
#'
#' @return The number of tokens in the corpus.
#' 
#' @seealso \code{\link{ntypes}} and \code{\link{type}}
#' 
#' @export
#' @docType methods
#' @rdname N-methods
#'
#' @examples
#' data(dickensFrequencyList)
#' N(dickensFrequencyList);
setGeneric("N", function(corpus) standardGeneric("N"));

##############################################################
#' Number of tokens of a corpus.
#'
#' @param corpus Any \code{\link{corpus}} concrete subclass.
#'
#' @param ... more arguments for the different implementations.
#'
#' @return The number of tokens in the corpus.
#' 
#' @seealso \code{\link{ntypes}} and \code{\link{type}}
#' 
#' @export
#' @docType methods
#' @rdname N-methods
#'
#' @examples
#' data(dickensFrequencyList)
#' N(dickensFrequencyList);
setGeneric("subcorpus", function(corpus, ...) {
  return(standardGeneric("subcorpus"));
})

