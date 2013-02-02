##############################################################
#' The Corpus class
#'
#' This virtual class is the root hierarchy of all classes available for representing
#' linguistic corpora.
#'
#' Subclass of \code{Corpus} must implement the generic methods \code{\link{N}}.
#'
#' \itemize{
#'   \item \textbf{\code{Corpus}}.\\
#'         Method: \code{\link{N}}.
#'   \itemize{
#'     \item \textbf{\code{\link{CorpusAsFrequencies}}}.\\
#'           Method: \code{\link{ntype}} and \code{\link{types}}.
#'       \itemize{
#'         \item \textbf{\code{\link{LexicalTable}}}
#'         \item \textbf{\code{\link{FrequencyList}}}
#'     }
#'     \item \textbf{\code{\link{CorpusAsTokens}}}.\\
#'           Method: \code{\link{ngram}}, \code{\link{kwic}}, \code{\link{tactique.subcorpus}} and \code{\link{slice}}.
#'       \itemize{
#'         \item \textbf{\code{\link{FullText}}}
#'         \item \textbf{\code{\link{Tabulated}}}
#'     }
#'   }
#' }
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

