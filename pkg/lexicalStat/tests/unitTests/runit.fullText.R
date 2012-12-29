test_readFile <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- lines2fullText(fileName);
  len <- length(c);
  checkEqualsNumeric(3, len);
}

test_readFiles <- function() {
  fileName1 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  fileName2 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small2.lines", package="lexicalStat")
  fileName3 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small3.lines", package="lexicalStat")
  c <- files2fullText(c(fileName1, fileName2, fileName3));
}

test_print_fullText <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- lines2fullText(fileName);
  print(c);
}

test_summary_fullText <- function() {
  fileName <- system.file(c("exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- lines2fullText(fileName);
  summary(c);
}

test_as_fullText_tabulated <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- lines2fullText(fileName);
  t <- asTabulated(c);
  d <- asFullText(t, "word", "Part");
  checkEqualsNumeric(length(c), length(d));
  checkEquals(length(c), length(d));
}
