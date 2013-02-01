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

setMethod("lstructural", "TabulatedDataFrame", function(obj) obj@structural)

setMethod("lpositional", "TabulatedDataFrame", function(obj) obj@positional)

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

