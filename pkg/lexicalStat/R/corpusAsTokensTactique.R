############################################################
##
##
## regrouping tokens according to their position.
##
##
############################################################

##############################################################
setMethod("tactique.subcorpus", "FullTextList", function(corpus, slice, nslice, word, method) {
  if (slice > nslice) {
    stop("'slice' cannot be greater than 'nslice'");
  }
  if (!is.character(word)) {
    stop("'word' must be a character vector");
  }
  if (length(word) > 1) {
    stop("'word' must be a vector of length 1");
  }
  if (word == "") {
    stop("'word' cannot be an empty string");
  }
  
  words.by.slices.by.parts <- .get.words.by.slices.by.parts(corpus, nslice=nslice);
  words.by.parts.by.slices <- .flip.inner.outer.parts(words.by.slices.by.parts, nslice);
  subcorpus <- .tactique.subcorpus(words.by.parts.by.slices, slice, word, method);
  return(subcorpus);
});

# TODO : to be implemented for Tabulated

##############################################################
setMethod("slice", "FullTextList", function(corpus, n) {
  .get.words.by.slices(corpus, n);
});

# TODO : to be implemented for Tabulated

## TODO
 #
 #
 # Given a corpus with parts and a lexical type,
 # get the distance to the closer type and to the
 # the begning of its part for each occurrences.
 #
 #
 ##
setGeneric("tactique.dist", function(corpus, type) {
  return(standardGeneric("tactique.dist"));
})

############################################################
##
##
## Private
##
##
############################################################

##
 #
 #
 #
 #
 #
 ##
# input: words.by.slice: a list where each item (part) is a character vector of words
# words.by.parts == list(
#   part1=c(wordA1, wordA2, wordA3, wordA4),
#   part2=c(wordB1, wordB2, wordB3, wordB4)
# );
# return:
# words.by.slices == list(
#   part1=c(wordA1, wordA2, wordB1, wordB2),
#   part2=c(wordA3, wordA4, wordB3, wordB4)
# );
.get.words.by.slices <- function(words.by.parts, nslice=10) {
#  print("a");
  words.by.slices.by.parts <- .get.words.by.slices.by.parts(words.by.parts, nslice=nslice);
#  print("b");
  words.by.parts.by.slices <- .flip.inner.outer.parts(words.by.slices.by.parts, nslice);
#  print("c");
  words.by.slices <- .remove.inner.part(words.by.parts.by.slices);
#  print("d");
  names(words.by.slices) <- paste("slice", 1:nslice);
#  print("e");
  x <- fullText(words.by.slices);
  return(x);
}

##
 #
 #
 #
 #
 #
 ##
# each part is character vector of words:
# words.by.parts == list(
#   part1=c(word1, word2),
#   part2=c(word1, word2)
# );
# make each part a list of slices, i.e. character vector of words:
# words.by.slices.by.parts == list(
#   part1=list(
#     slice1=c(word1, word2),
#     slice2=c(word1, word2)
#     ),
#   part2=list(
#     slice1=c(word1, word2),
#     slice2=c(word1, word2)
#     )
# );
# devide each part in tactique
.get.words.by.slices.by.parts <- function(words.by.parts, nslice=10, min=2*nslice) {
  part.length <- sapply(words.by.parts, length);
  words.by.parts <- words.by.parts[part.length >= min];

  lapply(
      words.by.parts,
      function (part) {
          nwords <- length(part);
          if (nwords < nslice) {
              tactique <- as.list(part);
              tactique[(nwords+1):10] <- "";
              return(tactique);
          }
          part.size <- nwords %/% nslice;
          mod <- nwords %% nslice;
          part.size <- rep(part.size, nslice);
          if (mod > 0) {
              part.size[1:mod] <- part.size[1:mod] + 1;
          }
          word2groups.index <- rep(1:nslice, times=part.size);
          tactique <- split(part, word2groups.index);
          return(tactique);
      }
      );
}

############################################################
##
## Dealing with variation in partition depth.
##
############################################################

##
 #
 #
 #
 #
 #
 ##
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
.flip.inner.outer.parts <- function(tokens.by.subparts.by.parts, nsubpart=10) {
  check.subpart.length <- sapply(tokens.by.subparts.by.parts, function(part) {
      length(part) == nsubpart;
  });
  if (!all(check.subpart.length)) {
    stop(paste("all part must contains", nsubpart, "subpart"));
  }

  words.by.parts.by.subparts <- lapply(
      1:nsubpart,
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

##
 #
 #
 #
 #
 #
 ##
# keep only the division in subpart:
.remove.inner.part <- function(tokens.by.parts.by.subparts) {
  tokens.by.subparts <- lapply(tokens.by.parts.by.subparts, function(x) {unlist(x, use.names=FALSE)});
  return(tokens.by.subparts);
}


##
 #
 #
 # return a FrequencyList
 #
 #
 ##
# slice : the number of the slice we are interested in
# mode : how to extract the corpus ofr computing specificities: see get.sub.corpus.in.tactique
# corpus <- list(
# slice1=list(
#   part1=c("un", "deux"),
#   part2=c("trois", "deux")),
# slice2=list(
#   part1=c("trois", "quatre"),
#   part2=c("quatre", "cinq")),
# slice3=list(
#   part1=c("un", "trois"),
#   part2=c("trois", "quatre")
# )
# )
.tactique.subcorpus <- function(words.by.parts.by.slices, slice, word, method="ps(w)") {

  #
  # checking consistency of data
  #

  nslice <- length(words.by.parts.by.slices);
  if (slice > nslice) {
    stop(paste("max:", nslice));
  }
  if (!is.character(words.by.parts.by.slices[[1]][[1]])) {
    stop("The content of the parts must be character vector");
  }

  nparts.by.slices <- sapply(words.by.parts.by.slices, length);
  nparts <- nparts.by.slices[1];
  if (any(nparts.by.slices != nparts)) {
    stop(paste("Not all slices contain", nparts, "parts"));
  }

  #
  # building the sub-corpus
  #

  souscorpus <- .get.sub.corpus.in.tactique(words.by.parts.by.slices, word=word, slice=slice, method="ps(s,w)");
  if (length(souscorpus[[1]]) == 0) {
    stop("No article contain the requested word");
  }
  n.rejected.article <- sum(sapply(souscorpus[[1]], is.null));
  if (n.rejected.article > 0) {
    print(paste("No occurrence of word \"", word, "\" in", n.rejected.article, " parts (slice", slice, ")"));
  }
  print(paste(nparts, "parts in total"));

  if (n.rejected.article == nparts) {
    stop("No article contain the requested word");
  }

  souscorpus <- unlist(souscorpus); # NULL are disgarded thanks to unlist()
  if (length(souscorpus) == 0) stop("empty subcorpus");

  souscorpus.fl <- table(souscorpus);
  if (any("" == names(souscorpus.fl))) {
    souscorpus.fl <- souscorpus.fl[-which("" == names(souscorpus.fl))];
  }
  return(frequencyList(souscorpus.fl));
}

##
 #
 #
 #
 #
 #
 ##
# Extract a set of parts in a corpus where token are grouped into parts grouped into slice.
# Extraction is made according to a word and a slice number
# The "method" tell how to use this arguments:
# - c
#   the corpus, any argument are ignored
# - s
#   the given slice in the corpus, the "word" argument is ignored
# - ps(w)
#   all the slices * parts containing the word at any slice
#   (the slice argument is ignored)
# - ps(s,w)
#   all the slices * parts at a given slice containing the word
# - p(s,w)
#   all the (complete) parts containing the word at the given slice
# - p(w)
#   all the parts containing the word at any slice
#
# ps(w) et ps(s,w) retournent des NULL.
.get.sub.corpus.in.tactique <- function(words.by.parts.by.slices, word, slice, method) {
  if (method == "c") {
    return(words.by.parts.by.slices);
  } else if (method == "s") {
    return(words.by.parts.by.slices[[slice]]);
  } else if (method == "p(s,w)") {
    sliced <- words.by.parts.by.slices[[slice]];
    contain <- sapply(
        sliced,
        function(article) {if (word %in% article) return(TRUE) else return(FALSE)}
        );
    returned <- lapply(
      words.by.parts.by.slices,
      function (sliced) {
      sliced[contain];
      }
      );
    return(returned);
  } else if (method == "p(w)") {
    contain.word <- logical(length(words.by.parts.by.slices[[1]]));
    for (s in 1:length(words.by.parts.by.slices)) {
      a.slice <- words.by.parts.by.slices[[s]];
      cont <- sapply(
          a.slice,
          function(an.article) {
          if (word %in% an.article) return(TRUE) else return(FALSE)
          }
          );
      contain.word <- contain.word | cont;
    }
    returned <- lapply(
        words.by.parts.by.slices,
        function (sliced) {
        sliced[contain.word];
        }
        );
    return(returned);
  } else if (method == "ps(s,w)") {
    sliced <- words.by.parts.by.slices[[slice]];
    returned <- lapply(
        sliced,
        function(article) {if (word %in% article) return(article) else return(NULL)}
        );
    returned <- returned[! sapply(returned, is.null)];
    return(list(slice=returned));
  } else if (method == "ps(w)") {
    returned <- lapply(
        words.by.parts.by.slices,
        function(a.slice) {
        lapply(
          a.slice,
          function(an.article) {
          if (word %in% an.article) return(an.article) else return(NULL)
          }
          )
        }
        );
     returned <- lapply(returned, function(my.slice) {
         my.slice[! sapply(my.slice, is.null)]
     });
     returned <- returned[sapply(returned, length) != 0];
     return(returned);
  } else {
    stop("unknown method");
  }
}
