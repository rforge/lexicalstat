# Virtual class Corpus and methods common to all Corpus sub-classes.

setClass("Corpus");

setIs("FrequencyList", "Corpus");
setIs("LexicalTable", "Corpus");
setIs("FullText", "Corpus");
setIs("Tabulated", "Corpus");

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
## ntype
##
##
############################################################

setGeneric("ntype", function(obj, positional) standardGeneric("ntype"));

setMethod("ntype", "FrequencyList", function(obj) nrow(obj));

setMethod("ntype", "LexicalTable", function(obj) nrow(obj));

setMethod("ntype", c("Tabulated", "character"), function(obj, positional) nlevels(obj[,positional]));

setMethod("ntype", "FullText", function(obj) length(unique(unlist(obj))));

############################################################
##
##
## types
##
##
############################################################

setGeneric("types", function(obj, positional) standardGeneric("types"));

setMethod("types", "FrequencyList", function(obj) sort(as.character(obj$type)));

setMethod("types", "LexicalTable", function(obj) sort(rownames(obj)));

setMethod("types", c("Tabulated", "character"), function(obj, positional) sort(levels(obj[,positional])));

setMethod("types", "FullText", function(obj) sort(unique(unlist(obj))));

