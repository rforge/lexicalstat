
##
## n-gram as a string, where grams are concatenated with "."
##

# the part too short are deleted
ngram.parts <- function(token.by.part, n=3, sep=".") {
  print("vérif");
  parts.ok <- sapply(token.by.part, function(x) length(x) > n);
  print("extrait les parties correctes");
  parts <- token.by.part[parts.ok];
  print("traite les parties");
  lapply(parts, ngram, n, sep);
}

ngram <- function(tokens, n=3, sep=".") {
 idx <- .nseq(length(tokens), n);
 ngram <- tokens[idx];
 ngram <- split(ngram, rep(1:length(tokens), each=n));
 ngram <- sapply(ngram, paste, collapse=sep);
 return(ngram);
}

##
## n-gram as a matrix, with n-columns
##

ngram.matrix.parts <- function(token.by.part, n=3) {
  print("vérif");
  parts.ok <- sapply(token.by.part, function(x) length(x) > n);
  print("extrait les parties correctes");
  parts <- token.by.part[parts.ok];
  print("traite les parties");
  lapply(parts, ngram.matrix, n);
}

ngram.matrix <- function(tokens, n=3) {
 return(.get.mat(tokens, n));
}

.nseq <- function(l, n) {
  if (l <= n) {
    stop("l must be greater than n");
  }
  x <- rep(1:l, each = n);
  for (i in 2:n) {
    is <- seq(i,l*n,n);
    #print("--");
    #print(i);
    #print(is);
    x[is] <- x [is] + (i-1);
  }
  return(x);
}

.old.ngram <- function(tokens, n=3, sep=".") {
 if (n >= length(tokens)) {
   stop("ngram length must be smaller than the number of tokens");
 }
 if (!is.character(tokens)) {
   stop("tokens must be a character vector");
 }
 m <- .get.mat(tokens, n); 
 gram <- apply(m, 1, paste, collapse=sep);
 return(gram);
}

.get.mat <- function(tokens, n) {
  m <- matrix(c(rep(tokens, n), rep("", n)), nrow=length(tokens)+1, ncol=n);
  m <- m[1:(nrow(m)-n),];
  return(m);
}

