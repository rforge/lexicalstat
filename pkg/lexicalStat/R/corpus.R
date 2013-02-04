##############################################################
#' The Corpus class
#'
#' This virtual class is the root hierarchy of all classes available for representing
#' linguistic corpora.
#'
#' Subclass of \code{Corpus} must implement the generic methods \code{\link{N}}.
#'
#' Here is an overview of the class you may use for representing corpora:
#'
#' \itemize{
#'   \item \bold{\code{Corpus}}. \emph{Method}: \code{\link{N}}.
#'   \itemize{
#'     \item \bold{\code{\link{CorpusAsFrequencies}}}. \emph{Method}: \code{\link{ntype}} and \code{\link{types}}.
#'       \itemize{
#'         \item \bold{\code{\link{FrequencyList}}}. The list of total frequencies of types.
#'         \item \bold{\code{\link{LexicalTable}}}. A table of sub-frequencies (types * parts).
#'     }
#'     \item \bold{\code{\link{CorpusAsTokens}}}. \emph{Method}: \code{\link{ngram}}, \code{\link{kwic}}, \code{\link{tactique.subcorpus}} and \code{\link{slice}}.
#'       \itemize{
#'         \item \bold{\code{\link{FullText}}}. A list of vector (part) of characters (tokens).
#'         \item \bold{\code{\link{Tabulated}}}. Tokens represented in several ways (lemma, pos...) with several possible grouping of tokens (
#'     }
#'   }
#' }
#'
#' The virtual class \code{\link{CorpusAsFrequencies}} groups classes where the actual
#' exhaustive list of tokens is not kept, byt where frequencies are given instead.
#'
#' The virtual class \code{\link{CorpusAsTokens}} groups classes where actual tokens are
#' available in their textual order.
#'
#' Each class is more suited for some task and more suited for reading a corpus from a text
#' file: \link{FullText}} make easy to read corpus as plain text files; \code{\link{Tabulated}}
# 'make easy to read corpus as a tabulated file (as typically produced by part-of-speech tagger).
#'
#' The \code{\link{FrequencyList}} is the less rich representation. It does not contains any partition of
#' of the corpus in part. Both \code{\link{LexicalTable}} and \code{\link{FullText}} contains a partition
#' of the corpus. \code{\link{Tabulated}} is the most rich representation, it may give several representation
#' for each token and several groupings.
#'
#' A set of \code{\link{as.*}} functions allows for conversion between classes:
#' \code{\link{as.FrequencyList}},
#' \code{\link{as.LexicalTable}}, \code{\link{as.Tabulated}} and \code{\link{as.FullText}}. Of
#' course, conversion may only be run from the "richer" representation toward the less “rich". That is:
#'
#' You can convert \code{\link{FullText}} and \code{\link{Tabulated}} back of forth. You can convert both
#' into \code{\link{LexicalTable}}. You can convert a \code{\link{LexicalTable}} only into a \link{FrequencyList}}
#' and a \link{FrequencyList}} into nothing else:
#'
#' (\code{\link{FullText}} <-> \code{\link{Tabulated}}) -> \code{\link{LexicalTable}} -> \link{FrequencyList}}
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

