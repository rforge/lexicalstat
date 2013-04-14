##############################################################
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

##############################################################
# TODO : as.character(types) : trancher si les listes de formes sont plutôt des vecteurs caractères ou des facteurs.
wordAssociation <- function(N, n, K, k, measure="wam.specificities", types=NULL, parts=NULL) {

  if (!is.character(types)) {
    stop("types must be a character vector or NULL");
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

# TODO : not the same semantic as other N()

setMethod("N", "WordAssociation", function(corpus) corpus@N)

# TODO : not the same semantic as other N()

setMethod("types", "WordAssociation", function(corpus) corpus@types)

setGeneric("n", function(obj) {
  return(standardGeneric("n"));
})

setMethod("n", "WordAssociation", function(obj) obj@n)

setGeneric("K", function(obj) {
  return(standardGeneric("K"));
})

setMethod("K", "WordAssociation", function(obj) obj@K)

setGeneric("k", function(obj) {
  return(standardGeneric("k"));
})

setMethod("k", "WordAssociation", function(obj) obj@k)

setGeneric("k", function(obj) {
  return(standardGeneric("k"));
})

setMethod("k", "WordAssociation", function(obj) obj@k)

setGeneric("association", function(obj) {
  return(standardGeneric("association"));
})

setMethod("association", "WordAssociation", function(obj) obj@association)

setGeneric("indicator.name", function(obj) {
  return(standardGeneric("indicator.name"));
})

setMethod("indicator.name", "WordAssociation", function(obj) obj@indicator.name)

setGeneric("parts", function(obj) {
  return(standardGeneric("parts"));
})

setMethod("parts", "WordAssociation", function(obj) obj@parts)

