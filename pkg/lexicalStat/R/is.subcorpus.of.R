############################################################
##
## Implementation of methods for deciding if a corpus is a subset of another
##
############################################################

setGeneric("is.subcorpus.of", function(subcorpus, corpus) {
  return(standardGeneric("is.subcorpus.of"));
})

setMethod("is.subcorpus.of", c("FrequencyList", "FrequencyList"), function(subcorpus, corpus) {
  t <- types(subcorpus);
  if (! all(has.types(corpus, t))) {
    return(FALSE);
  }

  if (any(frequencies(subcorpus, t) > frequencies(corpus, t))) {
    return(FALSE);
  }
  return(TRUE);
});

