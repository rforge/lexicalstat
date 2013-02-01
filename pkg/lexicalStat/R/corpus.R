# Virtual class Corpus and methods common to all Corpus sub-classes.

setClass("Corpus");

setGeneric("N", function(obj) standardGeneric("N"));

############################################################
##
##
## ntype
##
##
############################################################

setGeneric("ntype", function(obj, positional) standardGeneric("ntype"));

############################################################
##
##
## types
##
##
############################################################

setGeneric("types", function(obj, positional) standardGeneric("types"));

