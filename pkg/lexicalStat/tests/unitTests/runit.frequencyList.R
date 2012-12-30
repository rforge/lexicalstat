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

test_frequencyList_print <- function() {
	c <- LETTERS[sample(1:20, 20, replace=T)];
	f <- table(c)
	fl <- frequencyList(f);
	print(fl);
}

############################################################
##
## As functions
##
############################################################

# TODO

############################################################
##
## is.subcorpus.of
##
############################################################

# TODO
