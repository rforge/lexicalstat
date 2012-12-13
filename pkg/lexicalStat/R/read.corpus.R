read.corpus.txt <- function(file, enc, skipEmpty=FALSE) {
  corpus <- readLines(file, encoding=enc);
# remove empty lines
  index_empty_lines <- grep("^\\W*$", corpus);
  if (length(index_empty_lines) > 0) {
    if (skipEmpty) {
      corpus <- corpus[-index_empty_lines];
    } else {
      stop("Empty lines found in the text file")
    }
  }
  return(corpus);
}

read.corpus.txts <- function(files, enc, skipEmpty=FALSE) {
   corpus <- vector(mode="list", length=length(files));
   for (i in 1:length(files)) {
       corpus[[i]] <- read.corpus.txt(files[i], enc, skipEmpty);
   }
   return(corpus);
}

