setClass(
    "WordAssociation",
    representation(
      N = "numeric",
      n = "numeric",
      K = "numeric",
      k = "numeric",
      association="matrix",
      indicator.name="character",
      types="character",
      parts="character"
      ),
    );

##
 #
 # Four vector of same length ; may be recycled.
 # Each element of these vector correspond to a form.
 #
 # N : size (number of tokens) of the corpus
 # n : size (number of tokens) of the sub-corpus
 # K : Number of occurrences of a form in the corpus
 # k : Number of occurrences of a form in the subcorpus
 #
 ##
# TODO : as.character(types) : trancher si les listes de formes sont plutôt des vecteurs caractères ou des facteurs.
wordAssociation <- function(N, n, K, k, measure="wam.specificities", types, parts) {

  if (!is.character(types)) {
    stop("types must be a character vector");
  }

  if (!is.character(parts)) {
    stop("parts must be a character vector");
  }
    
  if (is.null(N) | is.null(n) | is.null(K) | is.null(k)) {
    stop("none of the four arguments N, n, K, k can be null");
  }
  if (any(is.na(N)) | any(is.na(n)) | any(is.na(K)) | any(is.na(k))) {
    stop("none of the four arguments N, n, K, k can be NA");
  }

  # TODO : another mechanism of association name/function, in order for the user to be able to extend the list.
  if (any(!measure %in% c("wam.MI", "wam.fisher", "wam.specificities", "wam.binomial", "wam.loglikelihood"))) {
    stop(paste("unknown measure: ", paste(measure, collapse=" "), sep=""));
  }

  association <- matrix(0, nrow=max(length(N), length(n), length(K), length(k)), ncol=length(measure));
  colnames(association) <- measure;
  for (m in measure) {
  # TODO: if several measure, the creation of the data.frame in "m" is done several time...
    indicateurs <- do.call(m, list(N, n, K, k));
    association[,m] <- indicateurs;
  }

  return(new("WordAssociation",
         N=N, n=n, K=K, k=k,
         association=association,
         indicator.name=measure,
         types=types,
         parts=parts));
}

##
 #
 # N
 #
 ##
 # TODO : not the same semantic as other N()
#setGeneric("N", function(obj) {
#  return(standardGeneric("N"));
#})

setMethod("N", "WordAssociation", function(obj) obj@N)

##
 #
 # n
 #
 ##
setGeneric("n", function(obj) {
  return(standardGeneric("n"));
})

setMethod("n", "WordAssociation", function(obj) obj@n)

##
 #
 # K
 #
 ##
setGeneric("K", function(obj) {
  return(standardGeneric("K"));
})

setMethod("K", "WordAssociation", function(obj) obj@K)

##
 #
 # k
 #
 ##
setGeneric("k", function(obj) {
  return(standardGeneric("k"));
})

setMethod("k", "WordAssociation", function(obj) obj@k)


##
 #
 # k
 #
 ##
setGeneric("k", function(obj) {
  return(standardGeneric("k"));
})

setMethod("k", "WordAssociation", function(obj) obj@k)

##
 #
 # association
 #
 ##
setGeneric("association", function(obj) {
  return(standardGeneric("association"));
})

setMethod("association", "WordAssociation", function(obj) obj@association)

##
 #
 # indicator.name
 #
 ##
setGeneric("indicator.name", function(obj) {
  return(standardGeneric("indicator.name"));
})

setMethod("indicator.name", "WordAssociation", function(obj) obj@indicator.name)

##
 #
 # types
 #
 ##
# TODO : attention même nom mais pas même sémantique que la fonction "typ" 
# setGeneric("types", function(obj) {
#   return(standardGeneric("types"));
# })

setMethod("types", "WordAssociation", function(obj) obj@types)

##
 #
 # parts
 #
 ##
setGeneric("parts", function(obj) {
  return(standardGeneric("parts"));
})

setMethod("parts", "WordAssociation", function(obj) obj@parts)

