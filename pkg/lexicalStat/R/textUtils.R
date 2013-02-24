##############################################################
line2paragraph <- function(lines) {
  if(!is.character(lines)) {
    stop("lines must be a character vector");
  }
  is.empty <- grep("^\\s*$", lines);
  if (length(is.empty) == 0) {
    return(paste(lines, collapse=" "));
  }
  if (max(is.empty) < length(lines)) {
    is.empty <- append(is.empty, length(lines));
  }

  len <- is.empty - c(0, is.empty[-length(is.empty)])
  parts <- rep(1:length(is.empty), times=len);
  x <- split(lines, parts);
  y <- sapply(x, paste, collapse=" ");
  return(y);
}

