##############################################################
#
setClass("Tabulated", contains = "CorpusAsTokens");

# TODO : faire une fonction pour créer une partition sur la base de la position dans une autre partition

#
#
# cwb-like representation : un vecteur forme, un vecteur index de partie...
#
#

# A tabulated corpus is a data frame where:
# - each line represent a token;
# - some columns represent various aspect of the token (such as inflected form, pos, lemma);
# - some other columns are numeric and regroup range of consecutive tokens by giving them
#   a common id.

##############################################################
setGeneric("lstructural", function(corpus) {
  return(standardGeneric("lstructural"));
})

##############################################################
setGeneric("lpositional", function(corpus) {
  return(standardGeneric("lpositional"));
})

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

