test_word.associations_sumary <- function() {
	data(dickensLexicalTable);
	w <- wam(dickensLexicalTable);
	summary(w);
}

test_word.association_reduce.matrix <- function() {
   m <- matrix(1:12, nrow=6, ncol=2);
   colnames(m) <- c("One", "Two");
   m <- m[order(m[,"One"], decreasing=TRUE),]

   res <- lexicalStat:::.matrix.head.tail(m, 1, 5, 0);
   checkTrue(nrow(res) == 5);
}

test_word.association_reduce.matrix_bigger_to <- function() {
   m <- matrix(1:12, nrow=6, ncol=2);
   colnames(m) <- c("One", "Two");
   m <- m[order(m[,"One"], decreasing=TRUE),]

   res <- lexicalStat:::.matrix.head.tail(m, 1, 15, 0);
   checkTrue(nrow(res) == 6);
}

test_word.association_reduce.matrix_with_negative <- function() {
   m <- matrix(1:12, nrow=6, ncol=2);
   colnames(m) <- c("One", "Two");
   m[4:6,1] <- - m[4:6,1];
   m <- m[order(m[,"One"], decreasing=TRUE),]

   res <- lexicalStat:::.matrix.head.tail(m, 1, 2, 3);
   checkTrue(nrow(res) == 4);
}

test_word.association_reduce.matrix_with_negative_bigger_to <- function() {
   m <- matrix(1:12, nrow=6, ncol=2);
   colnames(m) <- c("One", "Two");
   m[4:6,1] <- - m[4:6,1];
   m <- m[order(m[,"One"], decreasing=TRUE),]

   res <- lexicalStat:::.matrix.head.tail(m, 1, 7, 3);
   checkTrue(nrow(res) == 6);
}

test_word.association_reduce.matrix_with_negative_return_empty <- function() {
   m <- matrix(1:12, nrow=6, ncol=2);
   colnames(m) <- c("One", "Two");
   m[4:6,1] <- - m[4:6,1];
   m <- m[order(m[,"One"], decreasing=TRUE),]

   res <- lexicalStat:::.matrix.head.tail(m, 4, 6, 3);
   checkTrue(nrow(res) == 0);
}
