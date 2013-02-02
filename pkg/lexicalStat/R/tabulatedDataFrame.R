##############################################################
#' The TabulatedDataFrame class
#'
#' This class is the default implementation of \code{\link{Tabulated}}. It can be used
#' for representing corpus read from tabulated files as produced by most POS tagger.
#'
#' @name TabulatedDataFrame
#' @seealso \code{\link{Tabulated}}
#' @rdname TabulatedDataFrame
#' @aliases TabulatedDataFrame-class
#' @exportClass TabulatedDataFrame
#' @author Sylvain Loiseau
setClass("TabulatedDataFrame",
         representation(positional = "character", structural = "character"),
         contains = c("data.frame", "Tabulated"));

############################################################
##
## Implementation of Corpus
##
############################################################

##############################################################
#' @rdname N-methods
#' @aliases N,TabulatedDataFrame-method
setMethod("N", "TabulatedDataFrame", function(corpus) nrow(corpus));

# setMethod("ntype", c("TabulatedDataFrame", "character"), function(obj, positional) nlevels(obj[,positional]));

# setMethod("types", c("TabulatedDataFrame", "character"), function(obj, positional) sort(levels(obj[,positional])));

############################################################
##
## Implementation of Tabulated
##
############################################################

setMethod("lstructural", "TabulatedDataFrame", function(corpus) corpus@structural)

setMethod("lpositional", "TabulatedDataFrame", function(corpus) corpus@positional)

############################################################
##
## Utility functions
##
############################################################

setMethod("summary", signature(object = "TabulatedDataFrame"), function(object) {
  cat(paste("A corpus with", nrow(object), "tokens\n"));
  cat(paste("Positional attributes:", paste(lpositional(object), collapse=" "), "\n"));
  cat(paste("Structural attributes:", paste(lstructural(object), collapse=" "), "\n"));
  invisible(object);
});

