## Interface for computing word association measure with corpus classes
## (FullText, LexicalTable, Tabulated, FrequencyList)
##
## wam = Word association measure
##
## 1/ "wam.*" functions for each corpus data structure (fullText,
## lexicalTable, frequencyList, corpus).
##
## 2/ All these functions call wam.numeric() with four arguments.
##
## 3/ wam.numeric(), in turn, calls all one (or several) of the functions
## computing association measure (loglikelihood, binomial, specificities, etc).
## (see word.association.indicators.R)
##
## 4/ All this function return an object "WordAssociation" (see word
## word.association.R).
##
## 5/ This object can be printed with several options (see
## word.association.print.R).

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

setGeneric("wam", function(corpus, measure="specificities", types=NULL, parts=NULL, positional=NULL, structural=NULL, subcorpus=NULL) {
  return(standardGeneric("wam"));
})

## 
 # ------------------------------------------------------------------------
 # For "fullText" object
 # ------------------------------------------------------------------------
 ##
setMethod("wam", c("FullText"), function(corpus, measure, types) {
    m <- asLexicalTable(corpus);
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
setMethod("wam", "Tabulated", function(corpus, measure, types, positional, structural) {
  if (is.null(positional)) {
    stop("positional cannot be null");
  }
  if (is.null(structural)) {
    stop("structural cannot be null");
  }
  m <- asLexicalTable(corpus, positional, structural);
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
  if (is.missing(subcorpus)) {
    stop("'subcorpus' cannot be missing");
  }
  if (is.null(subcorpus)) {
    stop("'subcorpus' cannot be null");
  }

  if(!is.subcorpus.of(subcorpus, corpus)) {
    stop("'subcorpus' does not appear to be a subcorpus of 'corpus'");
  }

  N <- sum(corpus);
  n <- sum(subcorpus);
  k <- subcorpus;
  K <- corpus[ names(k) ];

  measured <- wam.num(N, n, K, k, measure);
  return(wordAssociation(N, n, K, k, measured, measure, names(subcorpus), "subcorpus"));
});

##
 # ------------------------------------------------------------------------
 # for "lexicalTable" object
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

  k <- as.vector(corpus);

  n <- as.vector(partMargin);
  names(n) <- names(partMargin);
  n <- rep(n, times=nrow(corpus));

  K <- as.vector(typeMargin);
  names(K) <- names(typeMargin);
  K <- rep(K, ncol(corpus));

  measured <- wam.num(N, n, K, k, measure);

  return(wordAssociation(N, n, K, k, measured, measure, names(K), names(n)));
});

############################################################
##
## 2/
##
##
## The generic method
##
##
############################################################

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
wam.num <- function(N, n, K, k, measure="specificities") {
  if (is.null(N) | is.null(n) | is.null(K) | is.null(k)) {
    stop("none of the four arguments N, n, K, k can be null");
  }
  if (any(is.na(N)) | any(is.na(n)) | any(is.na(K)) | any(is.na(k))) {
    stop("none of the four arguments N, n, K, k can be NA");
  }

  # TODO : another mechanism of association name/function, in order for the user to be able to extend the list.
  if (any(!measure %in% c("fisher", "specificities", "binomial", "loglikelihood"))) {
    stop(paste("unknown measure in ", paste(measure, collapse=" "), sep=""));
  }

  wa <- matrix(0, nrow=max(length(N), length(n), length(K), length(k)), ncol=length(measure));
  colnames(wa) <- measure;
  for (m in measure) {
    indicateurs <- do.call(m, list(N, n, K, k));
    wa[,m] <- indicateurs;
  }
  return(wa);
}

