.arg_notEmpty <- function(x) {
  if (length(x) == 0) {
    f <- as.character(sys.call(-1))[1]
    arg <- substitute(x);
    stop(paste("function '", f,"', arg. '", arg,"': vector must have at least one element", sep=""));
  }
}

.arg_length1 <- function(x) {
  if (length(x) != 1) {
    f <- as.character(sys.call(-1))[1]
    arg <- substitute(x);
    stop(paste("function '", f,"', arg. '", arg,"': must contains one element", sep=""));
  }
}

.arg_notNULL <- function(x) {
  if (is.null(x)) {
    f <- as.character(sys.call(-1))[1]
    arg <- substitute(x);
    stop(paste("function '", f,"', arg. '", arg,"': vector must not be null", sep=""));
  }
}

.arg_character <- function(x) {
  if (is.character(x)) {
    f <- as.character(sys.call(-1))[1]
    arg <- substitute(x);
    stop(paste("function '", f,"', arg. '", arg,"': must be a character vector", sep=""));
  }
}

.arg_logical <- function(x) {
  if (is.character(x)) {
    f <- as.character(sys.call(-1))[1]
    arg <- substitute(x);
    stop(paste("function '", f,"', arg. '", arg,"': must not be a logical vector", sep=""));
  }
}

.arg_numeric <- function(x) {
  if (is.character(x)) {
    f <- as.character(sys.call(-1))[1]
    arg <- substitute(x);
    stop(paste("function '", f,"', arg. '", arg,"': must not be a numeric vector", sep=""));
  }
}