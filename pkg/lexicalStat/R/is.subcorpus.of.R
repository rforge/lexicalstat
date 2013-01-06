############################################################
##
## Implementation of methods for deciding if a corpus is a subset of another
##
############################################################

setGeneric("is.subcorpus.of", function(subcorpus, corpus) {
  return(standardGeneric("is.subcorpus.of"));
})

setMethod("is.subcorpus.of", c("FrequencyList", "FrequencyList"), function(subcorpus, corpus) {
#  if (!class(subcorpus) == "frequencyList") stop("subcorpus must be a frequencyList");
#  if (!class(corpus) == "frequencyList") stop("corpus must be a frequencyList");
  if (! all(types(subcorpus) %in% types(corpus))) {
#    i <- names(subcorpus) %in% names(corpus);
#     stop(
#         paste(
#           sum(! i),
#           "types of the subcorpus not found in the corpus: ",
#           paste(names(subcorpus)[!i], collapse=", ")
#           )
#         );
    return(FALSE);
  }
  if (any(subcorpus[,2] > corpus[match(as.character(subcorpus[,1]), as.character(corpus[,1])),2])) {
    #stop("type cannot be more frequent in the subcorpus than in the corpus");
    return(FALSE);
  }
  return(TRUE);
});

