
test_readFile <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- lines2fullText(fileName);
  len <- length(c);
  checkEqualsNumeric(3, len);
}

test_readFile_depth <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- lines2fullText(fileName);
  d <- attr(c, "depth");
  checkEqualsNumeric(1, d);
}

test_readFile_nonexistent_file <- function() {
  fileName <- "foobar";
  checkException(lines2fullText(fileName));
}

test_readFiles <- function() {
  fileName1 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat");
  fileName2 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small2.lines", package="lexicalStat");
  fileName3 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small3.lines", package="lexicalStat");
  c <- files2fullText(c(fileName1, fileName2, fileName3));
}

test_readFiles_nonexistent_file <- function() {
  fileName1 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat");
  fileName2 <- "foobar";
  checkException(files2fullText(c(fileName1, fileName2)));
}

test_print.fullText <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- lines2fullText(fileName);
  print(c);
}

test_summary.fullText <- function() {
  fileName <- system.file(c("exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- lines2fullText(fileName);
  summary(c);
}

test_asFullText.tabulated <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- lines2fullText(fileName);
  t <- asTabulated(c);
  d <- asFullText(t, "word", "Part");
  checkEqualsNumeric(length(c), length(d));
  checkEquals(length(c), length(d));
}

# TODO

############################################################
##
## Tokenization
##
############################################################

test_tokenize <- function() {
  parts <- c("one two three", "four five");
  tokenized <- tokenize(parts);
  expected <- list(c("one", "two", "three"), c("four", "five"));
  checkEquals(expected, tokenized);
}

test_tokenize_with_empty_string <- function() {
  parts <- c("one two three", "four five", "");
  checkException(tokenize(parts));
}

# TODO

############################################################
##
## Dealing with variation in partition depth.
##
############################################################

# TODO

############################################################
##
## Filter, subcorpus
##
############################################################

# sample.lines contains:
#  First line with five tokens 
#  Second line is one tokens more
#  Third line and last line 
test_get.parts.containing.form <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "sample.lines", package="lexicalStat")
  c <- lines2fullText(fileName);
  d <- get.parts.containing.form(c, "tokens");
  checkEqualsNumeric(2, length(d));
}


test_get.parts.containing.all.form <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "sample.lines", package="lexicalStat")
  c <- lines2fullText(fileName);
  d <- get.parts.containing.all.forms(c, c("tokens", "Second"));
  checkEqualsNumeric(1, length(d));
}

# TODO