test_lexicalTable_creation_with_dense_matrix <- function() {
  m <- matrix(1:30, 3, 10);
  rownames(m) <- LETTERS[1:3];
  colnames(m) <- LETTERS[1:10];
  lt <- lexicalTable(m);
}

test_lexicalTable_creation_with_dense_matrix_norownames <- function() {
  m <- matrix(1:30, 3, 10);
  rownames(m) <- NULL
  checkException(lexicalTable(m));
}

test_lexicalTable_creation_with_dense_matrix_nocolnames <- function() {
  m <- matrix(1:30, 3, 10);
  colnames(m) <- NULL
  checkException(lexicalTable(m));
}

test_lexicalTable_creation_with_sparse_matrix <- function() {
# its a dgTMatrix, should be converted into dgCMatrix
  x <- spMatrix(ncol=10, nrow=10, i=1:10, 1:10, x=1:10);
  rownames(x) <- LETTERS[1:10];
  colnames(x) <- LETTERS[1:10];
  lt <- lexicalTable(x);
  checkEqualsNumeric(10, ncol(lt));
  checkEqualsNumeric(10, nrow(lt));
}

test_lexicalTable_creation_with_data.frame <- function() {
  x <- data.frame(1:10, 1:10)
  rownames(x) <- LETTERS[1:10];
  colnames(x) <- LETTERS[1:2];
  checkEqualsNumeric(2, ncol(x));
  checkEqualsNumeric(10, nrow(x));
}

############################################################
##
## Utility functions
##
############################################################

# TODO

############################################################
##
## read/write
##
############################################################

test_readLexicalTable <- function() {
  base <- paste(system.file(package = "lexicalStat"), "exempleData", "lemonde.lemme", sep="/")
  x <- readLexicalTable(base);
}

# TODO

############################################################
##
## As functions
##
############################################################

# TODO
