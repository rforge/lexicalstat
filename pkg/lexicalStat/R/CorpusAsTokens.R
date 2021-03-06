##############################################################
setClass("CorpusAsTokens", contains=c("Corpus", "VIRTUAL"));

##############################################################
setGeneric("ngram", function(corpus, n=3, sep=" ") {
  return(standardGeneric("ngram"));
})

##############################################################
setGeneric("tactique.subcorpus", function(corpus, slice=1, nslice=5, word, method="ps(s,w)") {
  return(standardGeneric("tactique.subcorpus"));
})

##############################################################
setGeneric("slice", function(corpus, n=10, ...) {
  return(standardGeneric("slice"));
})

##############################################################
setGeneric("kwic", function(corpus, pattern, left=20, right=20) {
  return(standardGeneric("kwic"));
})
