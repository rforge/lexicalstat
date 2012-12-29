
## Corpus as waw character vectors into a list.

# TODO :

# regrouper des fonctions subcorpus (cf. section ci-dessous, ainsi que dans tabulate) dans un subcorpus.R

# des fonctions pour regrouper différemment les parties... par exemple en fonction de la présence d'une forme. cf. specificities

# Reprendre des choses qui sont dans le package dico :
# library(dico)
# data(tlf_exe_lemma)
# bois <- lapply(tlf_exe_lemma, function(x) if ("femme" %in% x) return(x) else return(NULL))
# bois <- bois[!sapply(bois, is.null)]
# bois.lf <- table(unlist(bois))
# x <- frequencies_lemma["tlf_exe",,drop=FALSE]
# x <- rbind(x,x)
# rownames(x) <- c("tlf_exe", "bois")
# x[2,] <- 0
# x[2, names(bois.lf)] <- bois.lf
# x[1,] <- x[1,] - x[2,]
# x <- x[,colSums(x) > 0] 
# x <- t(x)
# specificites(x)

# Pouvoir calculer les spécificités facilement. Par exemple :
# specificites.fullText <- function(fullText, searched) {
# 	is.found <- lapply(fullText, function(x) if (lemme %in% x) return(TRUE) else return(FALSE));
# 	sous.corpus <- fullText[is.found];
# 	complement <- fullText[!is.found];
# 	sous.corpus.fl <- table(unlist(sous.corpus));
# 	complementaire.fl <- table(unlist(complementaire));
# 	forms <- unique(c(names(sous.corpus.fl), names(complementaire.fl)));
# 	m <- matrix(0, ncol=2, nrow=length(forms));
# 	colnames(m) <- c("sous-corpus", "complément");
# 	rownames(m) <- forms;
# 	m[names(sous.corpus.fl),] <- sous.corpus.fl;
# 	m[names(complementaire.fl),] <- complementaire.fl;
# }

############################################################
##
## Constructors/readers
##
############################################################

##
 #
 # Each line of a text file is tokenized and used as a part of the corpus
 #
 ##
lines2fullText <- function(file, enc="UTF-8", skipEmpty=FALSE) {
  if (!file.exists(file)) {
    stop("cannot read or access file");
  }
  lines <- readLines(file, encoding=enc);
  index_empty_lines <- grep("^\\W*$", lines);
  if (length(index_empty_lines) > 0) {
    if (skipEmpty) {
      lines <- lines[-index_empty_lines];
    } else {
      stop("Empty lines found in the text file")
    }
  }
  parts <- tokenize(lines);
  class(parts) <- "fullText";
  attr(parts, "depth") <- 1;
  return(parts);
}

#
# Each file of a group of text files is tokenized and used as a part of the corpus
#
files2fullText <- function(files, enc="UTF-8", skipEmpty=FALSE) {
  if (any(!file.exists(files))) {
    stop("cannot read or access some files");
  }
  parts <- vector(mode="list", length=length(files));
  for (i in 1:length(files)) {
    lines <- readLines(files[i], encoding=enc);
    index_empty_lines <- grep("^\\W*$", lines);
    if (length(index_empty_lines) > 0) {
      if (skipEmpty) {
	lines <- lines[-index_empty_lines];
      } else {
	stop("Empty lines found in the text file")
      }
    }
    part <- unlist(tokenize(lines));
    parts[[i]] <- part;
  }
  names(parts) <- files;
  class(parts) <- "fullText";
  attr(parts, "depth") <- 1;
  return(parts);
}

############################################################
##
## Utility functions
##
############################################################

print.fullText <- function(fullText) {
  summary(fullText);
  for (i in 1:min(length(fullText), 10)) {
    part <- fullText[[i]]
    tokens <- part[1:min(10, length(part))];
    cat(paste(paste(tokens, collapse=" "), "...\n", sep=""));
  }
}

summary.fullText <- function(fullText) {
  print(paste("A raw corpus with", length(fullText), "parts and ", sum(sapply(fullText, length)), "tokens"));
}

############################################################
##
## Conversion from other corpus classes
##
############################################################

asFullText <- function(x, ...) UseMethod("asFullText");

asFullText.tabulated <- function(corpus, positional, structural) {
  if (is.null(positional)) {
    stop("positional cannot be NULL");
  }
  if (is.null(structural)) {
    stop("structural cannot be NULL");
  }
# TODO check if structural and positional exist
  fullText <- split(corpus[,positional], corpus[,structural])
  fullText <- lapply(fullText, as.character);
  class(fullText) <- "fullText";
  attr(fullText, "depth") <- 1;
  return(fullText);
}

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
  if (!is.character(parts)) stop("part must be a character vector");
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
## Dealing with variation in partition depth.
##
############################################################

# Inverse the part/slice ordering. Suppose that each inner part have the same number of part.  make:
# input (e.g. the output of get.words.by.slices.by.parts()) : 
# words.by.slices.by.parts  == list(
#   partA1=list(
#     partB1=c(word1, word2),
#     partB2=c(word1, word2)
#     ),
#   partA2=list(
#     partB1=c(word1, word2),
#     partB2=c(word1, word2)
#     ),
#   partA3=list(
#     partB3=c(word1, word2),
#     partB4=c(word1, word2)
#     )
# );
#
# output :
# words.by.parts.by.slices == list(
#   list(
#     partA1=c(word1, word2),
#     partA2=c(word1, word2),
#     partA3=c(word1, word2)
#   ),
#   list(
#     partA1=c(word1, word2),
#     partA2=c(word1, word2),
#     partA3=c(word1, word2)
#   )
# );
flip.inner.outer.parts <- function(words.by.slices.by.parts, nslice=10) {
  check.slice.length <- sapply(words.by.slices.by.parts, function(part) {
      length(part) == nslice
  });
  if (!all(check.slice.length)) {
    stop(paste("all part must contains", nslice, "slice"));
  }

  words.by.parts.by.slices <- lapply(
      1:nslice,
      function(slice.idx, part) # slice.idx: index of a slice
      { 
      lapply(
        part,
        function(part, slice.idx) {
        if (length(part) < slice.idx) stop(paste("slice index too hight", slice.idx));
        return(part[[slice.idx]])
        },
        slice.idx)
      },
      words.by.slices.by.parts
      );
  return(words.by.parts.by.slices);
}

# keep only the division in slice:
# corpus == list(
#   slice1=c(word1, word2, word1, word2),
#   slice2=c(word1, word2, word1, word2)
# );
remove.inner.part <- function(words.by.parts.by.slices) {
  words.by.slices <- lapply(words.by.parts.by.slices, function(x) {unlist(x)});
  return(words.by.slices);
}


############################################################
##
## Filter, subcorpus
##
############################################################


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
