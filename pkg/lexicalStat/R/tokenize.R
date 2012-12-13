
tokenize <- function(parts) {
  .tokenize(parts, "[^A-Za-zéèêëÉÈÊËàâäÀÂÄôöÔÖùûüÙÛÜçÇîïÎÏ]+");
}

## "|" may be in a word
tokenize.treetagger <- function(parts) {
  .tokenize(parts, "[^A-Za-zéèêëÉÈÊËàâäÀÂÄôöÔÖùûüÙÛÜçÇîïÎÏ|]+");
}

## "|" may be in a word
tokenize.simple <- function(parts) {
  .tokenize(parts, "[ \\t\\.\\?!;:]+");
}

.tokenize <- function(parts, regexp) {
  if (!is.character(parts)) stop("part must be a character vector");
  corpus.tokenized <- strsplit(parts, regexp, perl=T);
  corpus.tokenized <- corpus.tokenized[sapply(corpus.tokenized, function(x) length(x) > 0)];
  corpus.tokenized <- lapply(corpus.tokenized, function(v) {  if (any(v == "")) v[-which(v=="")] else v });
  return(corpus.tokenized);
}
