##############################################################
#' The parent of classes representing corpus as a table of subfrequencies of types in parts of a corpus
#'
#' This virtual class is a subclass of \code{\link{CorpusAsFrequencies}} and the root
#' hierarchy of all classes representing corpora as subfrequencides of tokens in parts.
#'
#' Subclass of \code{FrequencyList} must implement the generic methods
#' \code{\link{subfreq}}.
#'
#' @name LexicalTable
#' @seealso its superclass \code{\link{CorpusAsFrequencies}}; its sibling \code{\link{FrequencyList}}; one of its implementation: \code{\link{LexicalTableSparseMatrix}}. \code{\link{Corpus}} for an overview of the available classes.
#' @rdname LexicalTable
#' @aliases LexicalTable-class
#' @exportClass LexicalTable
#' @author Sylvain Loiseau
setClass("LexicalTable", contains = "CorpusAsFrequencies");


##############################################################
#' Get the subfrequencies of given linguistic types in given types.
#'
#' @param corpus Any \code{\link{LexicalTable}} concrete subclass.
#'
#' @param types The linguistic types for which the subfrequencies are requested.
#'
#' @param parts The parts for which the subfrequencies are requested.
#'
#' @return A data.frame where columns = (Type, Part, Subrequency).
#'
#' @export
#' @docType methods
#' @rdname subfreq-methods
#' @genericMethods
#'
#' @examples
#' data(dickensLexicalTable)
#' subfreq(dickensLexicalTable, "the", "3");
setGeneric("subfreq", function(corpus, types, parts) standardGeneric("subfreq"));

############################################################
##
##
## Constructor
##
##
############################################################

lexicalTable <- function(mat) {
  if (is.data.frame(mat)) {
    mat <- as.matrix(mat);
    if (!is.numeric(mat)) {
      stop("cannot create lexicalTable with a non-numeric data.frame");
    }
  }
  m <- 0;
  if (is(mat, "sparseMatrix")) {
    m <- as(mat, "dgCMatrix");
  } else if (is.matrix(mat)) {
    m <- Matrix(mat, sparse = TRUE);
  } else if (class(mat) == "frequencyList") {
     stop("Not implemented yet");
  } else if (class(mat) == "fullText") {
      stop("Not implemented yet");
  } else if (class(mat) == "tabulated") {
      stop("Not implemented yet");
  } else {
    stop(paste("don't know how to make a lexicalTable with a", class(mat)));
  }

  if (is.null(rownames(m))) {
    stop("must have rownames");
  }
  if (is.null(colnames(m))) {
    stop("must have colnames");
  }

  return(new("LexicalTableSparseMatrix", m));
}
