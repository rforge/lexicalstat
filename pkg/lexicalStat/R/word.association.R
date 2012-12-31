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

wordAssociation <- function(N, n, K, k, association, indicator.name, types, parts) {
  return(new("WordAssociation", N=N, n=n, K=K, k=k, association=association, indicator.name=indicator.name, types=types, parts=parts));
}

##
 #
 # N
 #
 ##
setGeneric("N", function(obj) {
  return(standardGeneric("N"));
})

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
setGeneric("types", function(obj) {
  return(standardGeneric("types"));
})

setMethod("types", "WordAssociation", function(obj) obj@types)

setGeneric("types<-", function(x, types) standardGeneric("types<-"))

setReplaceMethod("types", "WordAssociation", function(x, types) {
x@types <- types
x
});

##
 #
 # parts
 #
 ##
setGeneric("parts", function(obj) {
  return(standardGeneric("parts"));
})

setMethod("parts", "WordAssociation", function(obj) obj@parts)

setGeneric("parts<-", function(x, parts) standardGeneric("parts<-"))

setReplaceMethod("parts", "WordAssociation", function(x, parts) {
x@parts <- parts
x
});
