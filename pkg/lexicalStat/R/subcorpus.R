############################################################
##
## Implementation of methods for filtering a corpus
##
############################################################

setGeneric("subcorpus", function(corpus, ...) {
  return(standardGeneric("subcorpus"));
})

setMethod("subcorpus", c(corpus="FrequencyList"), function(corpus, min.frequency) {
  x <- corpus[corpus >= min.frequency];
  return(frequencyList(x));
});

# , context.size=NULL

setMethod("subcorpus", c(corpus="FullText"), function(corpus, type) {
	contains.type <- lapply(corpus, function(x) type %in% x);
	return(corpus[contains.type]);
});

setMethod("subcorpus", c(corpus="FullText"), function(corpus, structural, positional=NULL, value=NULL) {
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
