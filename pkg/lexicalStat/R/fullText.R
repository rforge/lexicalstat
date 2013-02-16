##############################################################
#' The parent of classes representing corpus as a list of character (tokens) vectors (= parts)
#'
#' This virtual class is a subclass of \code{\link{CorpusAsTokens}} and the root
#' hierarchy of all classes representing corpora as sequences of character strings.
#'
#' @name FullText
#' @seealso its superclass \code{\link{CorpusAsTokens}}; its sibling \code{\link{Tabulated}}; one of its implementation: \code{\link{FullTextList}}. \code{\link{Corpus}} for an overview of the available classes.
#' @rdname FullText
#' @aliases FullText-class
#' @exportClass FullText
#' @author Sylvain Loiseau
setClass("FullText", contains="CorpusAsTokens");

############################################################
##
## Constructor
##
############################################################

fullText <- function(l, depth=1) {
  if(is.null(names(l))) {
    names(l) <- 1:length(l);
  }
  obj <- new("FullTextList", l, depth=depth);
  return(obj);
}

