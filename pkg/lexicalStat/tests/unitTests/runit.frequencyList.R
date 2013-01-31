test_frequencyList_frequencyList <- function() {
	x <- c(1, 2, 1, 2, 1, 3);
	names(x) <- LETTERS[1:length(x)];
	fl <- frequencyList(x);
}

test_frequencyList_frequencyList_nonames <- function() {
	x <- c(1, 2, 1, 2, 1, 3);
	checkException(frequencyList(x));
}

test_frequencyList_frequencyList_empty_names <- function() {
	x <- c(1, 2, 1, 2, 1, 3);
	names(x) <- c(LETTERS[1:(length(x)-1)], "");
	checkException(frequencyList(x));
}

test_frequencyList_frequencyList_not_numeric <- function() {
	checkException(frequencyList(LETTERS[1:10]));
}

test_frequencyList_frequencyList2 <- function() {
	c <- LETTERS[sample(1:20, 20, replace=T)];
	f <- table(c)
	fl <- frequencyList(f);
}

############################################################
##
## Accessors specific to this class (see also corpus.R for accessors common to other Corpus classes)
##
############################################################

test_frequencyList_freq <- function() {
  data(dickensFrequencyList)
  types <- c("doubt", ".", "the");
  expected <- c(1, 3, 4)
  found <- freq(dickensFrequencyList, types);
  checkEquals(expected, found);
}

test_frequencyList_freq_withNA <- function() {
  data(dickensFrequencyList)
  types <- c("doubt", ".", "the", "blablabla");
  expected <- c(1, 3, 4, NA)
  found <- freq(dickensFrequencyList, types);
  checkEquals(expected, found);
}

test_frequencyList_contains.types <- function() {
  data(dickensFrequencyList)
  types <- c("doubt", ".", "the", "blablabla");
  expected <- c(TRUE, TRUE, TRUE, FALSE);
  found <- contains.types(dickensFrequencyList, types);
  checkEquals(expected, found);
}

############################################################
##
## Utility functions
##
############################################################

test_frequencyList_print <- function() {
	c <- LETTERS[sample(1:20, 20, replace=T)];
	f <- table(c)
	fl <- frequencyList(f);
	print(fl);
}

