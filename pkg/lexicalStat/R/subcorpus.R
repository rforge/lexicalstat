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

setMethod("subcorpus", c(corpus="Tabulated"), function(corpus, structural, positional=NULL, value=NULL) {
  if (! structural %in% lstructural(corpus)) {
    stop("structural attribute not found");
  }
  if (is.null(positional) & is.null(value)) {
    p <- corpus[,structural];
    return(corpus[ p != -1 , ]);
  } else if (!is.null(positional) & !is.null(value)) {
    if (! positional %in% lpositional(corpus)) {
      stop("positional attribute not found");
    }
    p <- corpus[,structural];
    a <- corpus[,positional];
    ids <- unique(p[ a == value ]);
    return(corpus[ p == ids , ]);
  } else {
    stop("both 'positional' and 'value' must be NULL, or none of them");
  }

});
