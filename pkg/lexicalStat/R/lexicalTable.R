setClass("LexicalTable");

setIs("LexicalTable", "Corpus");

############################################################
##
##
## Return a data.frame where columns = types, parts, subfreq.
##
##
############################################################

setGeneric("subfreq", function(obj, types, parts) standardGeneric("subfreq"));

