##############################################################
#
setClass("Tabulated", contains=c("CorpusAsTokens", "VIRTUAL"));

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

tabulated <- function(m, structural.attributes=0) {
  debug <- FALSE;
  if (!is.data.frame(m)) {
    stop("m must be a data.frame");
  }
  c <- colnames(m);

  if (!is.numeric(structural.attributes)) {
    stop("'structural.attributes' must be numeric");
  }
  if (structural.attributes < 0) {
    stop("'structural.attributes' must be greater or equals to '0'");
  }
  if (! (length(colnames) <= structural.attributes)) {
    stop("The data.frame 'm' or all column are structural.attributes. A least one column must be for positional attribute");
  }

  are.factor <- sapply(m, is.factor);
  if (!all(are.factor)) {
    stop("'m' must have factor columns");
  }
  

  if (debug) print(paste("[tabulated] 1", date()))
  if (structural.attributes > 0) {
    if (debug) print(paste("[tabulated] 1a", date()))
    structural.attribute.name <- c[(length(c) - structural.attributes + 1):length(c)];
    # TODO check that ids are consecutive.
  } else {
    structural.attribute.name <- character(0);
  }

  if (debug) print(paste("[tabulated] 2", date()))
  positional.attribute.name <- c[1:(length(c) - structural.attributes)];

  if (debug) print(paste("[tabulated] 3", date()))
  obj <- new("TabulatedDataFrame", m, positional=positional.attribute.name, structural=structural.attribute.name);
  if (debug) print(paste("[tabulated] 4", date()))
  return(obj);
}

