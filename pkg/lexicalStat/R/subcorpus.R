
get.parts.with.token.fl <- function(tokens.by.part, token) {
  subcorpus <- get.parts.with.token(tokens.by.part, token);
  subcorpus.fl <- table(unlist(subcorpus));
  return(subcorpus.fl);
}

get.parts.with.token <- function(tokens.by.part, token) {
  contain.token <- sapply(tokens.by.part, function(part) token %in% part);
  subcorpus <- tokens.by.part[contain.token];
  return(subcorpus);
}

get.parts.with.tokens <- function(tokens.by.part, tokens) {
  contain.token <- sapply(tokens.by.part, function(part) all(tokens %in% part));
  subcorpus <- tokens.by.part[contain.token];
  return(subcorpus);
}
