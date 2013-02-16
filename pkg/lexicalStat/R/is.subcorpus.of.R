############################################################
##
## Implementation of methods for deciding if a corpus is a subset of another
##
############################################################

#' @genericMethods
setGeneric("is.subcorpus.of", function(subcorpus, corpus) {
  return(standardGeneric("is.subcorpus.of"));
})

setMethod("is.subcorpus.of", c("FrequencyList", "FrequencyList"), function(subcorpus, corpus) {
  t <- types(subcorpus);
  if (! all(contains.types(corpus, t))) {
    return(FALSE);
  }

  if (any(freq(subcorpus, t) > freq(corpus, t))) {
    return(FALSE);
  }
  return(TRUE);
});

