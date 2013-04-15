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

wam <- function(N, n, K, k, measure=wam.loglikelihood, types=NULL, parts=NULL, ...) {

  types <- as.character(types)
  if (!is.character(types)) {
    stop("types must be a character vector or NULL");
  }

  parts <- as.character(parts)
  if (!is.character(parts)) {
    stop("parts must be a character vector");
  }
    
  if (is.null(N) | is.null(n) | is.null(K) | is.null(k)) {
    stop("none of the four arguments N, n, K, k can be null");
  }
  if (any(is.na(N)) | any(is.na(n)) | any(is.na(K)) | any(is.na(k))) {
    stop("no element of the four arguments N, n, K, k can be NA");
  }

  association <- matrix(0, nrow=max(length(N), length(n), length(K), length(k)), ncol=length(measure));
  measure <- c(measure); # otherwise "objet de type 'closure' non indiçable" error with length(measure) = 1
  i <- 1;
  for (m in measure) {
    if (any(! c("N", "n", "K", "k") == names(formals(m))[1:4] ))  {
      stop(paste( "function", name, "do not accept the N, n, K, k arguments"));
    }
    indicateurs <- m(N, n, K, k, ...);
    association[,i] <- indicateurs;
    i <- i + 1;
  }
  colnames(association) <- 1:length(measure);

  return(new("WordAssociation",
         N=N, n=n, K=K, k=k,
         association=association,
         indicator.name=as.character(1:length(measure)),
         types=types,
         parts=parts));
}

setGeneric("N", function(obj) {
  return(standardGeneric("N"));
})

setMethod("N", "WordAssociation", function(obj) obj@N)

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

setGeneric("types", function(obj) {
  return(standardGeneric("types"));
})

setMethod("types", "WordAssociation", function(obj) obj@types)

setGeneric("parts", function(obj) {
  return(standardGeneric("parts"));
})

setMethod("parts", "WordAssociation", function(obj) obj@parts)

