# TODO : faire une fonction pour créer une partition sur la base de la position dans une autre partition

#
#
# cwb-like representation : un vecteur forme, un vecteur index de
# partie...
#
#

# A tabulated corpus is a data frame where:
# - each line represent a token;
# - some columns represent various aspect of the token (such as inflected form, pos, lemma);
# - some other columns are numeric and regroup range of consecutive tokens by giving them
#   a common id.

setClass("Tabulated");

setIs("Tabulated", "CorpusLinear");

############################################################
##
##
## lstructural
##
##
############################################################

setGeneric("lstructural", function(obj) {
  return(standardGeneric("lstructural"));
})

############################################################
##
##
## lpositional
##
##
############################################################

setGeneric("lpositional", function(obj) {
  return(standardGeneric("lpositional"));
})

