##############################################################
#' Compute word association measure (wam) given a corpus object.
#'
#' @param corpus a \code{\link{Corpus}}
#'
#' @param subcorpus a \code{\link{Corpus}} representing a subset of the
#'        \code{Corpus} argument. For the Corpus with part
#'        (\code{\link{LexicalTable}, \code{\link{FullText}}), may be missing or NULL 
#'        (in which case the attracted forms are measured for each part) or numeric vector
#'        giving the index of the parts of the corpus to be used as subcorpus.
#'
#' @return Return an object \code{\link{WordAssociation}}
#' This object can be printed with several options (see function \code{\link{print.WordAssociation}}).
#' 
#' @export
#' @docType methods
#' @rdname wam-methods
#' @seealso \code{\link{WordAssociation}}, \code{\link{print.WordAssociation}}
#'
#' @examples
#' data(dickensFullText)
#' wam(dickensFullText, 1)
setGeneric("wam", function(corpus, subcorpus=NULL, measure="wam.specificities", ...) {
  return(standardGeneric("wam"));
})

##############################################################
#' @rdname wam-methods
#' @aliases wam,FullText,missing-method
setMethod("wam", c("FullText", "missing"), function(corpus, subcorpus, measure, types) {
  m <- as.LexicalTable(corpus);
  w <- wam(corpus=m, measure=measure, types=types);
  return(w);
});

##############################################################
#' @rdname wam-methods
#' @aliases wam,FullText,missing-method
setMethod("wam", c("FullText", "NULL"), function(corpus, subcorpus, measure, types) {
   wam(corpus=corpus, measure=measure, types=types);
});

##############################################################
#' @rdname wam-methods
#' @aliases wam,FullText,numeric-method
setMethod("wam", c("FullText", "numeric"), function(corpus, subcorpus, measure, types) {
  m <- as.FrequencyList(corpus);
  x <- as.FrequencyList(corpus[subcorpus]);
  w <- wam(corpus=m, subcorpus=x, measure=measure, types=types);
  return(w);
});

##############################################################
#' @rdname wam-methods
#' @aliases wam,Tabulated,missing-method
setMethod("wam", c("Tabulated", "missing"), function(corpus, subcorpus, measure,  positional="word", structural, types) {
  .arg_length1(positional) & .arg_character(positional)
  .arg_length1(structural) & .arg_character(structural)

  m <- as.LexicalTable(corpus, positional, structural);
  w <- wam(m, measure, types);
  return(w);
});

##############################################################
#' @rdname wam-methods
#' @aliases wam,Tabulated,NULL-method
setMethod("wam", c("Tabulated", "NULL"), function(corpus, subcorpus, measure,  positional="word", structural, types) {
  wam(corpus=corpus, measure=measure, positional=positional, structural=structural, types=types);
});

##############################################################
#' @rdname wam-methods
#' @aliases wam,Tabulated,Tabulated-method
setMethod("wam", c("Tabulated", "Tabulated"), function(corpus, subcorpus, measure,  positional="word", types) {
  fl <- as.FrequencyList(corpus, positional);
  sfl <- as.FrequencyList(subcorpus, positional);
  wam(corpus=fl, subcorpus=sfl, measure=measure, types=types);
});

##############################################################
#' @rdname wam-methods
#' @aliases wam,FrequencyList,FrequencyList-method
setMethod("wam", c("FrequencyList","FrequencyList"), function(corpus, subcorpus, measure, types) {
  if(!is.subcorpus.of(subcorpus, corpus)) {
    stop("'subcorpus' does not appear to be a subcorpus of 'corpus'");
  }

  N <- N(corpus);
  n <- N(subcorpus);
  k <- subcorpus[,2];
# Pb d'encapsulation!
# TODO utiliser freq(corpus, types(subcorpus)) mais ne respecterait pas l'ordre
  K <- corpus[ match(subcorpus[,1], corpus[,1]), 2 ];

  return(wordAssociation(N, n, K, k, measure, types(subcorpus), "subcorpus"));
});

# TODO : reference to concrete implementation of LexicalTableSparseMatrix
# TODO = parts => subcorpus

##############################################################
#' @rdname wam-methods
#' @aliases wam,FrequencyList,FrequencyList-method
setMethod("wam", "LexicalTable", function(corpus, subcorpus, measure, types) {

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
