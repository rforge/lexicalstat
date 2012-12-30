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
    is.structural.numeric <- sapply(m[,structural.attributes], is.numeric);
    if (!all(is.structural.numeric)) {
      stop("structural attributes must be numeric column-vector");
    }
    # TODO check that ids are consecutive.
  }

  is.positional.character <- sapply(m[,positional.attributes], is.factor);
  if (!all(is.positional.character)) {
    stop("positional attributes must be character column-vector");
  }

  attr(m, "positional") <- positional.attributes;
  attr(m, "structural") <- structural.attributes;

  class(m) <- c(class(m), "tabulated");
  return(m);
}

##
 # Keep the part with id != -1.
 # or
# Keep the _part_ containing at least one token whose _attribute_
# has _value_.
#
##
subcorpus <- function(tabulated, structural, positional=NULL, value=NULL) {
  if (! structural %in% attr(tabulated, "structural")) {
    stop("structural attribute not found");
  }
  if (is.null(positional) & is.null(value)) {
    p <- tabulated[,structural];
    return(tabulated[ p != -1 , ]);
  } else if (!is.null(positional) & !is.null(value)) {
    if (! positional %in% attr(tabulated, "positional")) {
      stop("positional attribute not found");
    }
    p <- tabulated[,structural];
    a <- tabulated[,positional];
    ids <- unique(p[ a == value ]);
    return(tabulated[ p == ids , ]);
  } else {
    stop("both 'positional' and 'value' must be NULL, or none of them");
  }
}

asTabulated <- function(x, ...) UseMethod("asTabulated");

asTabulated.fullText <- function(fullText) {
  x <- unlist(fullText);
  m <- data.frame(word=x);
  y <- sapply(fullText, length);
  m[, "part"] <- rep(0:(length(fullText) - 1), times=y);
  t <- tabulated(m, "word", "part");
  return(t);
}

