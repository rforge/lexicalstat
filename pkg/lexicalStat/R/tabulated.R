##############################################################
#' The parent of classes representing corpus as a list of tokens representend as tuples
#'
#' This virtual class is a subclass of \code{\link{CorpusAsTokens}}. Corpus of type
#' \code{\link{Tabulated}} give several representation for each tokens (such as
#' part of speech, inflected form, lemma) and each token may belong to named range 
#' of different kinds (such as a sentence, a paragraph, a chapter...)
#'
#' Each character representation of tokens is called a \emph{positional attribute}. Each kind of
#' token sequences is called a \emph{structural attribute}.
#
#' This corpus representation is inspired by corpus representation in the CWB Workbench corpus.
#'
#' Subclass of \code{FrequencyList} must implement the generic methods
#' \code{\link{subfreq}}.
#'
#' @name Tabulated
#' @seealso its superclass \code{\link{CorpusAsTokens}}; its sibling \code{\link{FullText}}; one of its implementation: \code{\link{TabulatedDataFrame}. \code{\link{Corpus}} for an overview of the available classes.
#' @seealso \url{http://cwb.sourceforge.net}
#' @rdname Tabulated
#' @aliases Tabulated-class
#' @exportClass Tabulated
#' @author Sylvain Loiseau
setClass("Tabulated", contains = "CorpusAsTokens");

# TODO : faire une fonction pour créer une partition sur la base de la position dans une autre partition

#
#
# cwb-like representation : un vecteur forme, un vecteur index de partie...
#
#

# A tabulated corpus is a data frame where:
# - each line represent a token;
# - some columns represent various aspect of the token (such as inflected form, pos, lemma);
# - some other columns are numeric and regroup range of consecutive tokens by giving them
#   a common id.

##############################################################
#' Get the list of the names of the structural attribute.
#'
#' @param corpus Any \code{\link{Tabulated}} concrete subclass.
#'
#' @return A character vector
#'
#' @export
#' @docType methods
#' @rdname lstructural-methods
#' @genericMethods
#'
#' @examples
#' data(dickensTabulated)
#' lstructural(dickensTabulated);
setGeneric("lstructural", function(corpus) {
  return(standardGeneric("lstructural"));
})

##############################################################
#' Get the list of the names of the positional attribute.
#'
#' @param corpus Any \code{\link{Tabulated}} concrete subclass.
#'
#' @return A character vector
#'
#' @export
#' @docType methods
#' @rdname lpositional-methods
#' @genericMethods
#'
#' @examples
#' data(dickensTabulated)
#' lpositional(dickensTabulated);
setGeneric("lpositional", function(corpus) {
  return(standardGeneric("lpositional"));
})

############################################################
##
## Constructor
##
############################################################

tabulated <- function(m, positional.attributes, structural.attributes) {
  if (!is.data.frame(m)) {
    stop("m must be a data.frame");
  }
  c <- colnames(m);

  if (length(positional.attributes) == 0) {
    stop("a tabulated corpus must contain at least one positional attribute");
  }
  if (any(positional.attributes != c[1:length(positional.attributes)])) {
    stop("some positional attributes not found (or in wrong order)");
  }

  if (length(structural.attributes) > 0) {
    if (any(structural.attributes != c[(length(positional.attributes)+1):length(c)])) {
      stop("some structural attributes not found (or in wrong order)");
    }
    is.structural.numeric <- sapply(m[,structural.attributes], is.factor);
    if (!all(is.structural.numeric)) {
      stop("structural attributes must be factor column-vector");
    }
    # TODO check that ids are consecutive.
  }

  is.positional.character <- sapply(m[,positional.attributes], is.factor);
  if (!all(is.positional.character)) {
    stop("positional attributes must be factor column-vector");
  }

  obj <- new("TabulatedDataFrame", m, positional=positional.attributes, structural=structural.attributes);
  return(obj);
}

