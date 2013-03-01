##############################################################
setGeneric("wam", function(corpus, subcorpus=NULL, measure="wam.specificities", for.types=NULL, ...) {
  return(standardGeneric("wam"));
})

##############################################################
setMethod("wam", c("FullText", "missing"), function(corpus, subcorpus, measure, for.types) {
  m <- as.LexicalTable(corpus);
  w <- wam(corpus=m, measure=measure, for.types=for.types);
  return(w);
});

##############################################################
setMethod("wam", c("FullText", "NULL"), function(corpus, subcorpus, measure, for.types) {
   wam(corpus=corpus, measure=measure, for.types=for.types);
});

##############################################################
setMethod("wam", c("FullText", "numeric"), function(corpus, subcorpus, measure, for.types) {
  m <- as.FrequencyList(corpus);
  # TODO : fullText() should be removed here when the [ operator will be overriden:
  x <- as.FrequencyList(fullText(corpus[subcorpus]));
  w <- wam(corpus=m, subcorpus=x, measure=measure, for.types=for.types);
  return(w);
});

##############################################################
setMethod("wam", c("Tabulated", "missing"), function(corpus, subcorpus, measure,  positional="word", structural, for.types) {
  .arg_length1(positional) & .arg_character(positional)
  .arg_length1(structural) & .arg_character(structural)

  m <- as.LexicalTable(corpus, positional, structural);
  w <- wam(m, measure, for.types=for.types);
  return(w);
});

##############################################################
setMethod("wam", c("Tabulated", "NULL"), function(corpus, subcorpus, measure,  positional="word", structural, for.types) {
  wam(corpus=corpus, measure=measure, positional=positional, structural=structural, for.types=for.types);
});

##############################################################
setMethod("wam", c("Tabulated", "Tabulated"), function(corpus, subcorpus, measure,  positional="word", for.types) {
  fl <- as.FrequencyList(corpus, positional);
  sfl <- as.FrequencyList(subcorpus, positional);
  wam(corpus=fl, subcorpus=sfl, measure=measure, for.types=for.types);
});

##############################################################
setMethod("wam", c("FrequencyList","FrequencyList"), function(corpus, subcorpus, measure, for.types) {
  if(!is.subcorpus.of(subcorpus, corpus)) {
    stop("'subcorpus' does not appear to be a subcorpus of 'corpus'");
  }

  N <- N(corpus);

  n <- N(subcorpus);
  
  t <- lexicalStat::types(corpus);

  K <- freq(corpus, t);

  k <- numeric(length(t));
  names(k) <- t
  tt <- lexicalStat::types(subcorpus);
  k[tt] <- freq(subcorpus, tt);

  return(wordAssociation(N, n, K, k, measure, t, "subcorpus"));
});

# TODO : reference to concrete implementation of LexicalTableSparseMatrix
# TODO = parts => subcorpus

##############################################################
setMethod("wam", "LexicalTable", function(corpus, subcorpus, measure, for.types=NULL) {

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
  if (! is.null(for.types)) {      
    if (is.character(for.types)) {
      if (is.null(rownames(corpus))) {
        stop("The lexical table has no row names and the 'for.types' argument is a character vector.");
      }
      if (! all(for.types %in% rownames(corpus))) stop(paste(
            "Some requested for.types are not known in the lexical table: ",
            paste(for.types[! (for.types %in% rownames(corpus))], collapse=" ")
            )
          ); 
    } else {
      if (any(for.types < 1)) stop("The row index must be greater than 0.");
      if (max(for.types) > nrow(corpus)) stop("Row index must be smaller than the number of rows.");
    }
    corpus <- corpus[for.types, , drop = FALSE];
    typeMargin <- typeMargin[for.types];
  }

  # Filter on parts to be considered.
  if (! is.null(subcorpus)) {      
    if (is.character(subcorpus)) {
      if (is.null(colnames(corpus))) {
        stop("The lexical table has no col names and the 'subcorpus' argument is a character vector.");
      }
      if (! all(subcorpus %in% colnames(corpus))) stop(paste(
            "Some requested 'subcorpus' are not known in the lexical table: ",
            paste(subcorpus[! (subcorpus %in% colnames(corpus))], collapse=" "))
          ); 
    } else {
      if (max(subcorpus) > ncol(corpus)) stop("Column index must be smaller than the number of cols.");
      if (any(subcorpus < 1)) stop("The col index must be greater than 0.");
    }
    corpus <- corpus[ , subcorpus, drop=FALSE];
    partMargin <- partMargin[subcorpus];
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
