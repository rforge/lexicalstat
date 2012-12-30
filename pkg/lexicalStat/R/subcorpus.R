############################################################
##
## Implementation of methods for filtering a corpus
##

# TODO : how to have nammed argument rather than x, y, z ?

############################################################

setGeneric("subcorpus", function(corpus, x, y, z) {
  return(standardGeneric("subcorpus"));
})

setMethod("subcorpus", c(corpus="FrequencyList", x="numeric", y="missing", z="missing"), function(corpus, x) {
  x <- corpus[corpus >= min.frequency];
  return(frequencyList(x));
});

##
 # Keep the part with id != -1.
 # or
# Keep the _part_ containing at least one token whose _attribute_
# has _value_.
#
##
setMethod("subcorpus", c("Tabulated", "character", "character", "character"), function(corpus, x, y=NULL, z=NULL) {
  positional <- x;
  structural <- y;
  value <- z;

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
});

