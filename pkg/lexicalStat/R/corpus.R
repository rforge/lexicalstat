# Virtual class Corpus and methods common to all Corpus sub-classes.

setClass("Corpus");

setIs("FrequencyList", "Corpus");
setIs("LexicalTable", "Corpus");
setIs("FullText", "Corpus");
setIs("Tabulated", "Corpus");

############################################################
##
##
## hapax
##
##
############################################################

setGeneric("hapax", function(obj) {
  return(standardGeneric("hapax"));
})

setMethod("hapax", "FrequencyList", function(obj) names(obj)[obj == 1] )

############################################################
##
##
## N
##
##
############################################################

setGeneric("N", function(obj) standardGeneric("N"));

setMethod("N", "FrequencyList", function(obj) sum(obj$frequency));

setMethod("N", "LexicalTable", function(obj) sum(obj));

setMethod("N", "Tabulated", function(obj) nrow(obj));

setMethod("N", "FullText", function(obj) sum(sapply(obj, length)));

############################################################
##
##
## V
##
##
############################################################

setGeneric("V", function(obj, positional) standardGeneric("V"));

setMethod("V", "FrequencyList", function(obj) nrow(obj));

setMethod("V", "LexicalTable", function(obj) nrow(obj));

setMethod("V", "Tabulated", function(obj, positional) nlevels(obj[,positional]));

setMethod("V", "FullText", function(obj) length(unique(unlist(obj))));

############################################################
##
##
## types
##
##
############################################################

setGeneric("types", function(obj, positional) standardGeneric("types"));

setMethod("types", "FrequencyList", function(obj) obj$type);

setMethod("types", "LexicalTable", function(obj) rownames(obj));

setMethod("types", "Tabulated", function(obj, positional) levels(obj[,positional]));

setMethod("types", "FullText", function(obj) unique(unlist(obj)));

