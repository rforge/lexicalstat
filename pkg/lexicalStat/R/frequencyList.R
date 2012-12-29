
vector2frequencyList <- function(x) {
  if (!is.numeric(x)) {
      stop("frequencyList needs an numeric vector");
  }
  if (is.null(names(x))) {
      stop("frequencyList needs an named vector");
  }
  if (any("" == names(x))) stop("empty string cannot be a type name");
  class(x) <- "frequencyList";
  return(x);
}

setMethod("frequencyList", signature(x="numeric"), vector2frequencyList);

print.frequencyList <- function(x) {
  cat(paste("A frequency list:\n"));
  cat(paste("Number of forms:", length(x), "\n"));
  cat(paste("Number of tokens:", sum(x), "\n"));
  cat(paste("Number of hapax:", sum(x == 1), "\n"));
  n.top.frequency <- min(10, length(x));
  top.frequencies <- sort(x, decreasing=TRUE)[1:n.top.frequency];
  cat(
      paste("Top frequencies :",
	paste(
	  paste(names(top.frequencies), " (", top.frequencies, ")", sep=""),
	  collapse=" "),
	"\n", sep="")
     );
  invisible(x);
}

asFrequencyList <- function(x, ...) UseMethod("asFrequencyList");

asFrequencyList.tabulated <- function(corpus, attribute) {
  freq <- table(corpus[,attribute]);
  return(vector2frequencyList(freq));
}

asFrequencyList.fullText <- function(fullText) {
  if (! "fullText" %in% class(fullText)) {
    stop("not a fullText object");
  }
  freq <- table(unlist(fullText));
  return(vector2frequencyList(freq));
}

setGeneric("frequencyList", function(x) {
  return(standardGeneric("frequencyList"));
})

lexicalTable2frequencyList <- function(x) {
   fl <- rowSums(x);
   n <- rownames(x);
   names(fl) <- n;
   return(frequencyList(fl));
}

setMethod("frequencyList", signature(x="LexicalTable"), lexicalTable2frequencyList);

.is.a.subcorpus.of <- function(subcorpus, corpus) {
  if (!class(subcorpus) == "frequencyList") stop("subcorpus must be a frequencyList");
  if (!class(corpus) == "frequencyList") stop("corpus must be a frequencyList");
  if (! all(names(subcorpus) %in% names(corpus))) {
    i <- names(subcorpus) %in% names(corpus);
    stop(
        paste(
          sum(! i),
          "types of the subcorpus not found in the corpus: ",
          paste(names(subcorpus)[!i], collapse=", ")
          )
        );
  }
  if (any(subcorpus > corpus[names(subcorpus)])) {
    stop("type cannot be more frequent in the subcorpus than in the corpus");
  }

  return(TRUE);
}

