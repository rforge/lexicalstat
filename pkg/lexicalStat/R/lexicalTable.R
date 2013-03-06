##############################################################
setClass("LexicalTable", contains=c("CorpusAsFrequencies", "VIRTUAL"));

##############################################################
setGeneric("subfreq", function(corpus, types, parts) standardGeneric("subfreq"));

############################################################
##
##
## Constructor
##
##
############################################################

lexicalTable <- function(mat) {
  if (is.data.frame(mat)) {
    mat <- as.matrix(mat);
    if (!is.numeric(mat)) {
      stop("cannot create lexicalTable with a non-numeric data.frame");
    }
  }
  m <- 0;
  if (is(mat, "sparseMatrix")) {
    m <- as(mat, "dgCMatrix");
  } else if (is.matrix(mat)) {
    m <- Matrix(mat, sparse = TRUE);
  } else if (class(mat) == "frequencyList") {
     stop("Not implemented yet");
  } else if (class(mat) == "fullText") {
      stop("Not implemented yet");
  } else if (class(mat) == "tabulated") {
      stop("Not implemented yet");
  } else {
    stop(paste("don't know how to make a lexicalTable with a", class(mat)));
  }

  if (is.null(rownames(m))) {
    stop("must have rownames");
  }
  if (is.null(colnames(m))) {
    stop("must have colnames");
  }

  return(new("LexicalTableSparseMatrix", m));
}
