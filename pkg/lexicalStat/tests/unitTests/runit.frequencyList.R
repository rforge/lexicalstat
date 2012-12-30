test_frequencyList_vector2frequencyList <- function() {
	x <- c(1, 2, 1, 2, 1, 3);
	names(x) <- LETTERS[1:length(x)];
	fl <- vector2frequencyList(x);
}

test_frequencyList_vector2frequencyList_nonames <- function() {
	x <- c(1, 2, 1, 2, 1, 3);
	checkException(vector2frequencyList(x));
}

test_frequencyList_vector2frequencyList_empty_names <- function() {
	x <- c(1, 2, 1, 2, 1, 3);
	names(x) <- c(LETTERS[1:(length(x)-1)], "");
	checkException(vector2frequencyList(x));
}

test_frequencyList_vector2frequencyList_not_numeric <- function() {
	checkException(vector2frequencyList(LETTERS[1:10]));
}

test_frequencyList_vector2frequencyList2 <- function() {
	c <- LETTERS[sample(1:20, 20, replace=T)];
	f <- table(c)
	fl <- vector2frequencyList(f);
}

test_frequencyList_print <- function() {
	c <- LETTERS[sample(1:20, 20, replace=T)];
	f <- table(c)
	fl <- vector2frequencyList(f);
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
## is.a.subcorpus.of
##
############################################################

# TODO