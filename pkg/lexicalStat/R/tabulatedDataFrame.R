setClass("TabulatedDataFrame",
         representation(positional = "character", structural = "character"),
         contains = "data.frame");

setIs("TabulatedDataFrame", "Tabulated")

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

############################################################
##
## Implementation of Corpus
##
############################################################

setMethod("N", "TabulatedDataFrame", function(obj) nrow(obj));

setMethod("ntype", c("TabulatedDataFrame", "character"), function(obj, positional) nlevels(obj[,positional]));

setMethod("types", c("TabulatedDataFrame", "character"), function(obj, positional) sort(levels(obj[,positional])));

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

