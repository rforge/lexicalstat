##############################################################
setClass("CorpusAsFrequencies", contains=c("Corpus", "VIRTUAL"));

##############################################################
setGeneric("ntype", function(corpus) standardGeneric("ntype"));

##############################################################
setGeneric("types", function(corpus) standardGeneric("types"));

