##############################################################
#' The CorpusAsTokens virtual class
#'
#' This virtual class is the root hierarchy of all classes representing corpora with
#' each actual tokens, in their textual order
#'
#' Subclass of \code{CorpusAsTokens} must implement the generic methods
#' \code{\link{ngram}}, \code{\link{tactique.subcorpus}} and \code{\link{slice}}.
#'
#' @name CorpusAsTokens
#' @seealso \code{\link{Corpus}} and the subclasses \code{\link{FullText}} and \code{\link{Tabulated}}
#' @rdname CorpusAsTokens
#' @aliases CorpusAsTokens-class
#' @exportClass CorpusAsTokens
#' @author Sylvain Loiseau
setClass("CorpusAsTokens", contains="Corpus");

##############################################################
#' Create ngram (all the sequences of consecutive n tokens)
#'
#' @param corpus Any \code{\link{CorpusAsTokens}} concrete subclass.
#'
#' @return A \code{\link{CorpusAsTokens}} object where the tokens are ngram of the
#' input corpus.
#' 
#' @export
#' @docType methods
#' @rdname ngram-methods
#'
#' @examples
#' data(dickensFullText)
#' ngram(dickensFullText);
#'
#' data(dickensTabulated)
#' ngram(dickensTabulated);
setGeneric("ngram", function(corpus, n=3, sep=" ") {
  return(standardGeneric("ngram"));
})

##############################################################
#' Select tokens according to their position (beginning or end of corpus parts)
#'
#' Given a corpus and a partition in parts, extract a subcorpus containing
#' all the tokens found in a given "slice" in the parts and containing
#' a given linguistic type.
#'
#' @param corpus Any \code{\link{CorpusAsTokens}} concrete subclass.
#'
#' @return A \code{\link{FullText}} object where the parts are grouping of "slices"
#' (tokens at specific position in the input corpus parts) containing a given word.
#' 
#' @export
#' @docType methods
#' @rdname tactique.subcorpus-methods
#'
#' @examples
#' # The first half of all parts in the corpus
#' data(dickensFullText)
#' tactique.subcorpus(dickensFullText, 1, 2);
#'
#' # The first half containing 'the' of all parts in the corpus
#' tactique.subcorpus(dickensFullText, 1, 2, "the");
setGeneric("tactique.subcorpus", function(corpus, slice=1, nslice=5, word, method="ps(s,w)") {
  return(standardGeneric("tactique.subcorpus"));
})

##############################################################
#' Reshape a corpus according to the positions of tokens (beginning or end of corpus parts)
#'
#' @param corpus Any \code{\link{CorpusAsTokens}} concrete subclass.
#'
#' @return A \code{\link{FullText}} object where the parts contains tokens at
#' specific position (i.e. in specific slices) in the input corpus parts.
#' 
#' @export
#' @docType methods
#' @rdname slice-methods
#'
#' @examples
#' data(dickensFullText)
#' print(dickensFullText)
#' slice(dickensFullText, 3);
setGeneric("slice", function(corpus, n=10) {
  return(standardGeneric("slice"));
})

##############################################################
#' Print context of a token in the "key word in context" (KWIC) form.
#'
#' @param corpus Any \code{\link{CorpusAsTokens}} concrete subclass.
#'
#' @param pattern A regexp for selecting tokens to be used as keyword.
#'
#' @param left Number of characters on the left
#'
#' @param right Number of characters to be printed on the right
#'
#' @return A character vector of kwic.
#' 
#' @export
#' @docType methods
#' @rdname kwic-methods
#'
#' @examples
#' data(dickensFullText)
#' kwic(dickensFullText, "the");
setGeneric("kwic", function(corpus, pattern, left, right) {
  return(standardGeneric("kwic"));
})
