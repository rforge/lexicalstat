##############################################################
# 'make easy to read corpus as a tabulated file (as typically produced by part-of-speech tagger).
setClass("Corpus");

##############################################################
setGeneric("N", function(corpus) standardGeneric("N"));
