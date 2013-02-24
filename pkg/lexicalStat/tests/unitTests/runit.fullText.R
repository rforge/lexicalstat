############################################################
##
## Utils
##
############################################################

test_print.fullText <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- readTexts(fileName);
  print(c);
}

test_summary.fullText <- function() {
  fileName <- system.file(c("exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- readTexts(fileName);
  summary(c);
}

############################################################
##
## Read
##
############################################################

test_readFile <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- readTexts(fileName);
  len <- length(c);
  checkEqualsNumeric(3, len);
}

test_readFile_depth <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- readTexts(fileName);
  d <- attr(c, "depth");
  checkEqualsNumeric(1, d);
}

test_readFile_nonexistent_file <- function() {
  fileName <- "foobar";
  checkException(readTexts(fileName));
}

test_readMultipleFiles <- function() {
  fileName1 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat");
  fileName2 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small2.lines", package="lexicalStat");
  fileName3 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small3.lines", package="lexicalStat");
  c <- readTexts(c(fileName1, fileName2, fileName3));
}

test_readMultipleFiles_nonexistent_file <- function() {
  fileName1 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat");
  fileName2 <- "foobar";
  checkException(readTexts(c(fileName1, fileName2)));
}

test_readFile_lines <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- readTexts(fileName, split.on="lines");
}

test_readFile_paragraphs <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "small.paragraphs", package="lexicalStat")
  c <- readTexts(fileName, split.on="paragraphs");
  checkEqualsNumeric(3, length(c));
}

test_readFile_sentences <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- readTexts(fileName, split.on="sentences");
}

test_readFiles_files <- function() {
  fileName1 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat");
  fileName2 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small2.lines", package="lexicalStat");
  fileName3 <- system.file(c("inst",  "exempleData"), "LeMondeEco.small3.lines", package="lexicalStat");
  c <- readTexts(c(fileName1, fileName2, fileName3), split.on="files");
}

test_readFiles_dir <- function() {
  dir <- system.file(c("inst",  "exempleData/zola"), package="lexicalStat");
  c <- readTexts(dir, is.dir=TRUE, pattern="*.txt", split.on="files");
}

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

# Function removed as for now.

# test_get.parts.containing.form <- function() {
#  fileName <- system.file(c("inst",  "exempleData"), "sample.lines", package="lexicalStat")
#  c <- readTexts(fileName);
#  d <- get.parts.containing.form(c, "tokens");
#  checkEqualsNumeric(2, length(d));
# }


# Function removed as for now.
# test_get.parts.containing.all.form <- function() {
#  fileName <- system.file(c("inst",  "exempleData"), "sample.lines", package="lexicalStat")
#  c <- readTexts(fileName);
#  d <- get.parts.containing.all.forms(c, c("tokens", "Second"));
#  checkEqualsNumeric(1, length(d));
# }

# TODO

############################################################
##
## Private
##
############################################################

test_filename_dir1 <- function() {
   filename <- lexicalStat:::.filename(".", is.dir=TRUE, pattern="*.R$")
   checkEquals(filename, "./doRUnit.R");
}

test_filename_dir2 <- function() {
   filename <- lexicalStat:::.filename(".", is.dir=TRUE, pattern=NULL)
   checkEquals(length(filename), 2);
}

test_filename_filename1 <- function() {
   filename <- lexicalStat:::.filename("doRUnit.R", FALSE, NULL)
   checkEquals(length(filename), 1);
}

test_filename_filename_exception_nonexistentfile <- function() {
   checkException(lexicalStat:::.filename("non_existent_file", FALSE, NULL));
}

test_filename_filename_exception_pattern_with_file <- function() {
   checkException(lexicalStat:::.filename("doRUnit.R", FALSE, pattern="*.R"));
}

test_filename_exception_no_file_found <- function() {
   checkEqualsNumeric(length(lexicalStat:::.filename(".", is.dir=TRUE, pattern=".null")), 0);
}

