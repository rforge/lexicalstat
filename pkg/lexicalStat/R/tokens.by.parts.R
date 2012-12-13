print.tokens.by.parts <- function(tokens.by.parts) {
  if (!is.list(tokens.by.parts)) stop("tokens.by.parts must be a list");
  untokenized <- untokenize(tokens.by.parts);
  print(untokenized);
}

untokenize <- function(tokens.by.parts) {
  if (!is.list(tokens.by.parts)) stop("tokens.by.parts must be a list");
  untokenized <- lapply(tokens.by.parts, paste, collapse=" ");
  return(untokenized);
}
