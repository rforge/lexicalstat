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

test_lexicalTable_subfreq <- function() {
  data(dickensLexicalTable);
  found <- subfreq(dickensLexicalTable, c("is", "it", "no", "of"), c("1", "2"));
  expected <- data.frame(
  Type=c("is", "it", "no", "of","is", "it", "no", "of"),
  Part=c("1", "1", "1", "1", "2", "2", "2", "2"),
  Subfrequency=c(1 , 0 , 1 , 0 , 0, 0, 0, 1)
  );
  checkEquals(found, expected);
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
  base <- paste(system.file(package = "lexicalStat"), "exempleData", "lemonde", sep="/")
  x <- readLexicalTable(base);
}

# TODO

