##############################################################
setClass("CorpusAsFrequencies", contains="Corpus");

##############################################################
setGeneric("ntype", function(corpus) standardGeneric("ntype"));

##############################################################
setGeneric("types", function(corpus) standardGeneric("types"));

