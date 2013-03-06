##############################################################
setClass("FullTextList",
         representation(depth = "numeric"),
         contains = c("list", "FullText"));

############################################################
##
## Implementation of Corpus
##
############################################################

##############################################################
setMethod("N", "FullTextList", function(corpus) sum(sapply(corpus, length)));

# setMethod("ntype", "FullTextList", function(obj) length(unique(unlist(obj))));

# setMethod("types", "FullTextList", function(obj) sort(unique(unlist(obj))));

############################################################
##
## Reader
##
############################################################

##
 #
 # Each line of a text file is tokenized and used as a part of the corpus
 #
 ##
readTexts <- function(filenames, is.dir=FALSE, pattern=NULL, split.on="lines", enc="UTF-8", skipEmpty=TRUE) {
  
  debug <- FALSE
  
  filenames <- .filename(filenames, is.dir, pattern);
  if (length(filenames) == 0) {
    stop("no files selected");
  }

# TODO : source d'erreur dans le fait d'enumerer les options ici
# (+ dans le message d'erreur) et encore ci-dessous dans le 'if-else'.
  if (!is.character(split.on)) {
    stop("'split.on' must be a character vector");
  }
  if (length(split.on) != 1) {
    stop("'split.on' must have a length of 1");
  }
  if (! split.on %in% c("lines", "files", "sentences", "paragraphs")) {
    stop("'parts' must be one of 'lines', 'files', 'sentences', 'paragraphs'");
  }

  files <- vector(mode="list", length=length(filenames));
  for (i in 1:length(filenames)) {
    lines <- readLines(filenames[i], encoding=enc);
    files[[i]] <- lines;
  }

  corpus <- list();
  if (split.on == "lines" || split.on == "files") {
  
# remove empty line and tokenize first
# bellow (§) empty lines must not be removed first since they are §-boundaries.
    files <- lapply(files, function(lines) {
      is.empty <- lines == "";
      if (any(is.empty)) {
        return(lines[-which(is.empty)])
      } else {
        return(lines)
      }
      });

  l <- sapply(files, length);
  is.empty <- l == 0;
  if (any(is.empty)) {
    files <- files[-which(is.empty)];
  }

  if (debug) print("---");
  if (debug) print(length(files));
  if (debug) print("---");

    for (i in 1:length(files)) {
      if (debug) print(filenames[i]);
      if (debug) print(length(files[[i]]));
      files[[i]] <- tokenize(files[[i]]);
    }

    if (split.on == "lines") {
      corpus <- unlist(files, recursive=FALSE);
    } else if (split.on == "files") {
      corpus <- lapply(files, unlist);
      names(corpus) <- filenames;
    }
  } else {
# tokenize after reshaping.
    if (split.on == "sentences") {
      corpus <- unlist(files);
      corpus <- lapply(corpus, function(x) {
	  strsplit(x, split="[^M]\\.+")
	  });
      corpus <- unlist(corpus);
    } else if (split.on == "paragraphs") {
      corpus <- unlist(files);
      corpus <- line2paragraph(corpus);
    } else {
      stop("Unkwown option");
    }
    if (skipEmpty) {
      corpus <- corpus[!corpus == ""];
    }
    corpus <- tokenize(corpus);
    if (debug) print(corpus[[1]]);
  }

  obj <- fullText(corpus);
  if (debug) print(obj[[1]]);
  return(obj);
}

############################################################
##
## Utility functions
##
############################################################

# setMethod("show", signature(object="FullText"), function(object) {
#  print(object);
# });

# setMethod("print", signature(x="FullText"), function(x) {
# });

setMethod("summary", signature(object = "FullTextList"), function(object){
  print(paste("A raw corpus with", length(object), "parts and ", sum(sapply(object, length)), "tokens"));
  invisible(object);
  cat("Sample:\n");
  for (i in 1:min(length(object), 10)) {
    part <- object[[i]]
    tokens <- part[1:min(10, length(part))];
    cat(paste(paste(tokens, collapse=" "), "...\n", sep=""));
  }
});

############################################################
##
## Tokenization
##
############################################################

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
  if (!is.character(parts)) stop("parts must be a character vector");
  if (any(parts == "")) stop("parts must not contain empty string");

  corpus.tokenized <- strsplit(parts, regexp, perl=T);
  index <- sapply(corpus.tokenized, function(x) {length(x) > 0});
  if (!is.logical(index)) {
    stop("index must be logical");
  }
  corpus.tokenized <- corpus.tokenized[index];
  corpus.tokenized <- lapply(corpus.tokenized, function(v) {  if (any(v == "")) v[-which(v=="")] else v });
  return(corpus.tokenized);
}

print.tokens.by.parts <- function(tokens.by.parts) {
  if (!is.list(tokens.by.parts)) stop("tokens.by.parts must be a list");
  untokenized <- untokenize.by.parts(tokens.by.parts);
  print(untokenized);
}

untokenize.by.parts <- function(tokens.by.parts) {
  if (!is.list(tokens.by.parts)) stop("tokens.by.parts must be a list");
  untokenized <- lapply(tokens.by.parts, paste, collapse=" ");
  return(untokenized);
}

############################################################
##
## Private
##
############################################################

# TODO : allow for recursive ?

.filename <- function(filenames, is.dir, pattern) {

  if (!is.dir && !is.null(pattern)) {
    stop("'pattern' can be used only if 'is.dir' is set to TRUE");
  }

  if (is.dir) {
    filenames <- list.files(filenames, pattern=pattern, full.names=TRUE);
    are.dir <- file.info(filenames)[, "isdir"];
    if (any(are.dir)) {
      filenames <- filenames[!are.dir];
    }
  }

  nonexistent <- !file.exists(filenames);
  if (any(nonexistent)) {
    stop(paste("cannot read or access file(s): ", paste(filenames[nonexistent], collapse=" "), sep=""));
  }
  
  return(filenames);
}
