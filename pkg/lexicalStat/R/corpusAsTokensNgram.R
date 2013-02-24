# TODO : test for the fastest implementation.

##############################################################
setMethod("ngram", "FullTextList", function(corpus, n, sep) {
  x <- do.ngram.implementation2(corpus, n, sep);
  y <- fullText(x);
  return(y);
});

# TODO : to be implemented for Tabulated

##
 #
 # ngram : implementation 1
 #
 ##
do.ngram.implementation1 <- function(tokens, n, sep) {
  parts.ok <- sapply(tokens, function(x) length(x) > n);
  parts <- tokens[parts.ok];
  lapply(parts, .create.ngram, n, sep);
}

.create.ngram <- function(tokens, n=3, sep=".") {
 idx <- .nseq(length(tokens), n);
 ngram <- tokens[idx];
 ngram <- split(ngram, rep(1:length(tokens), each=n));
 ngram <- sapply(ngram, paste, collapse=sep);
 return(ngram);
}

.nseq <- function(l, n) {
  if (l <= n) {
    stop("l must be greater than n");
  }
  x <- rep(1:l, each = n);
  for (i in 2:n) {
    is <- seq(i,l*n,n);
    x[is] <- x [is] + (i-1);
  }
  return(x);
}

##
 #
 # ngram : implementation 1
 # n-gram as a matrix
 #
 ##
do.ngram.implementation2 <- function(token.by.part, n=3, sep=" ") {
  parts.ok <- sapply(token.by.part, function(x) length(x) > n);
  parts <- token.by.part[parts.ok];
  x <- lapply(parts, .get.mat, n);
  y <- lapply(x, apply, 1, paste, collapse=sep);
  return(y);
}

.get.mat <- function(tokens, n) {
  m <- matrix(c(rep(tokens, n), rep("", n)), nrow=length(tokens)+1, ncol=n);
  m <- m[1:(nrow(m)-n),];
  return(m);
}
