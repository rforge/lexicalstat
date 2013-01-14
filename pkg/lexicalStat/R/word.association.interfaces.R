## Interface for computing word association measure with corpus classes
## (FullText, LexicalTable, Tabulated, FrequencyList)
##
## wam = Word association measure
##
## Functions for each corpus data structure (fullText,
## lexicalTable, frequencyList, corpus).
##
## All these functions return an object "WordAssociation" (see word
## word.association.R).
##
## This object can be printed with several options (see word.association.print.R).

############################################################
##
##
## "wam.*" : functions for different data structures representing a corpus
##
## All these functions have an argument, "corpus", an argument "measure"
## transmited to wam.numeric(), and an argument "types" for filtering the
## types on which the attraction measures are to be computed.
##
##
############################################################

setGeneric("wam", function(corpus, measure="wam.specificities", types=NULL, parts=NULL, positional=NULL, structural=NULL, subcorpus=NULL) {
  return(standardGeneric("wam"));
})

## 
 # ------------------------------------------------------------------------
 # For "fullText" object
 # ------------------------------------------------------------------------
 ##
setMethod("wam", c("FullText"), function(corpus, measure, types) {
    m <- as.LexicalTable(corpus);
    w <- wam(m, measure, types);
    return(w);
    });

##
 # ------------------------------------------------------------------------
 # for "tabulated" object
 # - positional = the column giving the forms (should inflected form, lemma, or
 # pos be used if they are available?)
 # - structural = the column giving the partition factor for the forms.
 # ------------------------------------------------------------------------
 ##
#TODO : would it be possible to have positional and structural in 2nd and 3rd position?
setMethod("wam", "Tabulated", function(corpus, measure, types, positional, structural) {
  if (is.null(positional)) {
    stop("positional cannot be null");
  }
  if (is.null(structural)) {
    stop("structural cannot be null");
  }
  m <- as.LexicalTable(corpus, positional, structural);
  w <- wam(m, measure, types);
  return(w);
})

##
 # ------------------------------------------------------------------------
 # For "frequencyList" object
 # TODO : problem with the formal name "corpus": actually it is the subcorpus
 # ------------------------------------------------------------------------
 ##
setMethod("wam", "FrequencyList", function(corpus, measure, types, subcorpus) {
  if (is.null(subcorpus)) {
    stop("'subcorpus' cannot be null");
  }

  if(!is.subcorpus.of(subcorpus, corpus)) {
    stop("'subcorpus' does not appear to be a subcorpus of 'corpus'");
  }

  N <- N(corpus);
  n <- N(subcorpus);
  k <- subcorpus[,2];
# TODO utiliser freq(corpus, types(subcorpus)) mais ne respecterait pas l'ordre
  K <- corpus[ match(subcorpus[,1], corpus[,1]), 2 ];

  return(wordAssociation(N, n, K, k, measure, types(subcorpus), "subcorpus"));
});

##
 # ------------------------------------------------------------------------
 # For "lexicalTable" object
 # wam called on a "lexicalTable" corpus
 # ------------------------------------------------------------------------
 ##
# , measure="character", types="character", parts="character"
setMethod("wam", "LexicalTable", function(corpus, measure, types, parts) {

  partMargin <- colSums(corpus);
  names(partMargin) <- colnames(corpus);
  typeMargin <- rowSums(corpus);
  names(typeMargin) <- rownames(corpus);

  if (any(typeMargin == 0)) {
    stop("Row without any occurrence");
  }
  if (any(partMargin == 0)) {
    stop("Column without any occurrence");
  }

  N <- sum(partMargin);

  # Filter on tokens to be considered.
  if (! is.null(types)) {      
    if (is.character(types)) {
      if (is.null(rownames(corpus))) {
        stop("The lexical table has no row names and the \"types\" argument is a character vector.");
      }
      if (! all(types %in% rownames(corpus))) stop(paste(
            "Some requested types are not known in the lexical table: ",
            paste(types[! (types %in% rownames(corpus))], collapse=" ")
            )
          ); 
    } else {
      if (any(types < 1)) stop("The row index must be greater than 0.");
      if (max(types) > nrow(corpus)) stop("Row index must be smaller than the number of rows.");
    }
    corpus <- corpus[types, , drop = FALSE];
    typeMargin <- typeMargin[types];
  }

  # Filter on parts to be considered.
  if (! is.null(parts)) {      
    if (is.character(parts)) {
      if (is.null(colnames(corpus))) {
        stop("The lexical table has no col names and the \"parts\" argument is a character vector.");
      }
      if (! all(parts %in% colnames(corpus))) stop(paste(
            "Some requested parts are not known in the lexical table: ",
            paste(parts[! (parts %in% colnames(corpus))], collapse=" "))
          ); 
    } else {
      if (max(parts) > ncol(corpus)) stop("Column index must be smaller than the number of cols.");
      if (any(parts < 1)) stop("The col index must be greater than 0.");
    }
    corpus <- corpus[ ,parts, drop=FALSE];
    partMargin <- partMargin[parts];
  }
  if (nrow(corpus) == 0 | ncol(corpus) == 0) {
    stop("The lexical table must contains at least one row and one column.");
  }

# vectorisation : creation of the four vectors N, n, K, k of same length

  k <- as.vector(corpus);

  N <- rep(N, length(k));

  n <- as.vector(partMargin);
  names(n) <- names(partMargin);
  n <- rep(n, times=rep(nrow(corpus), length(n)));

  K <- as.vector(typeMargin);
  names(K) <- names(typeMargin);
  K <- rep(K, ncol(corpus));

  return(wordAssociation(N, n, K, k, measure, names(K), names(n)));
});

############################################################
##
##
## The common method
##
##
############################################################

