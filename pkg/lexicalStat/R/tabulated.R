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

##
 # Keep the part with id != -1.
 # or
# Keep the _part_ containing at least one token whose _attribute_
# has _value_.
#
##
subcorpus <- function(tabulated, part, attribute=NULL, value=NULL) {
  if (is.null(attribute) & is.null(value)) {
    p <- tabulated[,part];
    return(tabulated[ p != -1 , ]);
  } else if (!is.null(attribute) & !is.null(value)) {
    p <- tabulated[,part];
    a <- tabulated[,attribute];
    ids <- unique(p[ a == value ]);
    return(tabulated[ p == ids , ]);
  } else {
    stop("both attribute and value must be NULL, or neither one nor the other");
  }
}

asTabulated <- function(x, ...) UseMethod("asTabulated");

asTabulated <- function(fullText) {
  x <- unlist(fullText);
  tabulated <- data.frame(word=x);
  y <- sapply(fullText, length);
  tabulated[, "Part"] <- rep(0:(length(fullText) - 1), times=y);
  class(tabulated) <- c(class(tabulated), "tabulated");
  return(tabulated);
}

