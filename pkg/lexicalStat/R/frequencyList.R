setClass("FrequencyList");

setIs("FrequencyList", "Corpus");

############################################################
##
##
## freq
##
##
############################################################

setGeneric("freq", function(obj, types) {
  standardGeneric("freq")
});

############################################################
##
##
## contains.types
##
##
############################################################

setGeneric("contains.types", function(obj, types) {
  standardGeneric("contains.types")
});

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

