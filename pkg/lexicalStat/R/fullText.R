
## Corpus as raw character vectors into a list.

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

setClass("FullText",
         representation(depth = "numeric"),
         contains = "list");

############################################################
##
## Constructor
##
############################################################

fullText <- function(l, depth=1) {
  obj <- new("FullText", l, depth=depth);
  return(obj);
}

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
  names(parts) <- 1:length(parts);
  obj <- fullText(parts);
  return(obj);
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
  obj <- fullText(parts);
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

setMethod("summary", signature(object = "FullText"), function(object){
  print(paste("A raw corpus with", length(object), "parts and ", sum(sapply(object, length)), "tokens"));
  invisible(object);
  cat("Sample:\n");
  for (i in 1:min(length(x), 10)) {
    part <- x[[i]]
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
## Dealing with variation in partition depth.
##
############################################################

# Inverse the part/subpart ordering. Suppose that each inner part have the same number of part.  make:
# input (e.g. the output of get.words.by.subparts.by.parts()) : 
# tokens.by.subparts.by.parts  == list(
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
# words.by.parts.by.subparts == list(
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
flip.inner.outer.parts <- function(tokens.by.subparts.by.parts, nsubpart=10) {
  check.subpart.length <- sapply(tokens.by.subparts.by.parts, function(part) {
      length(part) == nsubpart;
  });
  if (!all(check.subpart.length)) {
    stop(paste("all part must contains", nsubpart, "subpart"));
  }

  words.by.parts.by.subparts <- lapply(
      1:subpart,
      function(subpart.idx, part) # subpart.idx: index of a subpart
      { 
      lapply(
        part,
        function(part, subpart.idx) {
        if (length(part) < subpart.idx) stop(paste("subpart index too hight", subpart.idx));
        return(part[[subpart.idx]])
        },
        subpart.idx)
      },
      tokens.by.subparts.by.parts
      );
  return(words.by.parts.by.subparts);
}

# keep only the division in subpart:
# corpus == list(
#   subpart1=c(word1, word2, word1, word2),
#   subpart2=c(word1, word2, word1, word2)
# );
remove.inner.part <- function(tokens.by.parts.by.subparts) {
  tokens.by.subparts <- lapply(tokens.by.parts.by.subparts, function(x) {unlist(x)});
  return(tokens.by.subparts);
}

############################################################
##
## Filter, subcorpus
##
############################################################


# TO BE DELETED
#get.parts.with.token.fl <- function(tokens.by.part, token) {
#  subcorpus <- get.parts.with.token(tokens.by.part, token);
#  subcorpus.fl <- table(unlist(subcorpus));
#  return(subcorpus.fl);
#}

get.parts.containing.form <- function(tokens.by.part, form) {
  if (length(form) != 1) stop("form must have one element");
  if (!is.character(form)) stop("form must be a character vector");
  if (attr(tokens.by.part, "depth") != 1) stop("depth must be 1");
  contain.form <- sapply(tokens.by.part, function(part) form %in% part);
  if (all(!contain.form)) {
    stop("No occurrence found");
  }
  subcorpus <- tokens.by.part[contain.form];
  return(subcorpus);
}

get.parts.containing.all.forms <- function(tokens.by.part, forms) {
  if (!is.character(forms)) stop("token must be a character vector");
  if (attr(tokens.by.part, "depth") != 1) stop("depth must be 1");
  contain.form <- sapply(tokens.by.part, function(part) all(forms %in% part));
  if (all(!contain.form)) {
    stop("No occurrence found");
  }
  subcorpus <- tokens.by.part[contain.form];
  return(subcorpus);
}

##
 #
 # Get each occurrence found of a form with a given number of cooccurrent
 #
 ##
get.tokens.by.context.by.part <- function(tokens.by.part, form, span.size) {
  if (!is.list(tokens.by.part)) stop("tokens.by.part must be a list");
  subcorpus.by.context.by.part <- lapply(tokens.by.part, function(tokens) {
      idx <- which(form == tokens);
      if (length(idx) > 0) {
        contexts <- vector(mode="list", length=length(idx));
        for(i in 1:length(idx)) {
          j <- idx[i];
          id <- (j-span.size):(j+span.size);
          id <- id[id > 0 & id <= length(tokens)];
          contexts[[i]] <- tokens[id];
        }
        return(contexts);
      } else {
        return(NULL);
      }
  });
  subcorpus.by.context.by.part <- subcorpus.by.context.by.part[
    ! sapply(subcorpus.by.context.by.part, is.null)
    ];
  return(subcorpus.by.context.by.part);
}
