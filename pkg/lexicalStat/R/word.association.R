##
## wam = Word association measure
##
## 1/ A "wam.*" function for each corpus data structure (fullText, lexicalTable, frequencyList, corpus).
##
## 2/ All these call wam.numeric() with four arguments.
##
## 3/ wam.numeric(), in turn, calls all one (or several) of the functions
## computing attraction measure (loglikelihood, binomial, specificities, etc).
##


# TODO : dans l'impression : mettre option si "+" ou "-".


###########################################################################
##
## 1/
##
## "wam.*" : functions for different data structures representing a corpus
##
## All these functions have an argument, "corpus", an argument "measure"
## transmited to wam.numeric(), and an argument "types" for filtering the
## types on which the attraction measures are to be computed.
##
##
###########################################################################

wam <- function(corpus, ...) UseMethod("wam");

## 
 # ------------------------------------------------------------------------
 # For "fullText" object
 # ------------------------------------------------------------------------
 ##
wam.fullText <- function(corpus, measure="specificities", types=NULL) {
  m <- asLexicalTable(corpus);
  return (wam(m));
}

##
 # ------------------------------------------------------------------------
 # for "tabulated" object
 # - positional = the column giving the form (should inflected form, lemma, or
 # pos be used if they are available?)
 # - 
 # ------------------------------------------------------------------------
 ##
wam.tabulated <- function(corpus, positional="word", value=NULL, structural=NULL, measure="specificities", types=NULL) {
}

##
 # TODO : problem with the formal name "corpus": actually it is the subcorpus
 ##
wam.frequencyList <- function(corpus, corpusFrequencyList, measure="specificities", types=NULL) {
  if (!class(corpusFrequencyList) == "frequencyList") {
    stop("second argument must be a frequency list");
  }

  .is.a.subcorpus.of(corpus, corpusFrequencyList);

  N <- sum(corpusFrequencyList);
  n <- sum(corpus);
  k <- corpus;
  K <- corpusFrequencyList[names(k)];

  measured <- wam(N, n, K, k);

  attr(measured, "N") <- N;
  attr(measured, "n") <- n;
  attr(measured, "K") <- K;
  attr(measured, "k") <- k;
  attr(measured, "types") <- names(corpus);
  attr(measured, "parts") <- "souscorpus";
  attr(measured, "measure") <- measure;
  class(measured) <- "wam";
  return(measured);
} 

##
 # wam called on a "lexicalTable" corpus
 ##
wam.lexicalTable <- function(corpus, measure="specificities", parts=NULL, types=NULL) {

  lexicaltable <- x;

  partMargin <- colSums(lexicaltable);
  typeMargin <- rowSums(lexicaltable);

  if (any(typeMargin == 0)) {
    stop("Row without any occurrence");
  }
  if (any(partMargin == 0)) {
    stop("Column without any occurrence");
  }

  N <- sum(partMargin);

  # Filter on tokens to be considered.
  if (! is.null(types)) {      
    if (is.character(types)) {
      if (is.null(rownames(lexicaltable))) {
        stop("The lexical table has no row names and the \"types\" argument is a character vector.");
      }
      if (! all(types %in% rownames(lexicaltable))) stop(paste(
            "Some requested types are not known in the lexical table: ",
            paste(types[! (types %in% rownames(lexicaltable))], collapse=" ")
            )
          ); 
    } else {
      if (any(types < 1)) stop("The row index must be greater than 0.");
      if (max(types) > nrow(lexicaltable)) stop("Row index must be smaller than the number of rows.");
    }
    lexicaltable <- lexicaltable[types, , drop = FALSE];
    typeMargin <- typeMargin[types];
  }

  # Filter on parts to be considered.
  if (! is.null(parts)) {      
    if (is.character(parts)) {
      if (is.null(colnames(lexicaltable))) {
        stop("The lexical table has no col names and the \"parts\" argument is a character vector.");
      }
      if (! all(parts %in% colnames(lexicaltable))) stop(paste(
            "Some requested parts are not known in the lexical table: ",
            paste(parts[! (parts %in% colnames(lexicaltable))], collapse=" "))
          ); 
    } else {
      if (max(parts) > ncol(lexicaltable)) stop("Column index must be smaller than the number of cols.");
      if (any(parts < 1)) stop("The col index must be greater than 0.");
    }
    lexicaltable <- lexicaltable[ ,parts, drop=FALSE];
    partMargin <- partMargin[parts];
  }
  if (nrow(lexicaltable) == 0 | ncol(lexicaltable) == 0) {
    stop("The lexical table must contains at least one row and one column.");
  }

  k <- as.vector(lexicaltable);

  n <- as.vector(partMargin);
  n <- rep(n, times=nrow(lexicaltable);

  K <- as.vector(typeMargin);
  K <- rep(f, ncol(lexicaltable));

  measured <- wam(N, n, K, k);

  attr(measured, "N") <- N;
  attr(measured, "n") <- n;
  attr(measured, "K") <- K;
  attr(measured, "k") <- k;
  attr(measured, "types") <- names(K);
  attr(measured, "parts") <- names(n);
  attr(measured, "measure") <- measure;
  class(measured) <- "wam";
  return(measured);
}

###########################################################################
##
## 2/
##
##
## The generic method
##
##
###########################################################################

##
 #
 # Four vector of same length ; may be recycled.
 # Each element of these vector correspond to a form.
 #
 # N : size (number of tokens) of the corpus
 # n : size (number of tokens) of the sub-corpus
 # K : Number of occurrences of a form in the corpus
 # k : Number of occurrences of a form in the subcorpus
 #
 ##
wam.numeric <- function(N, n, K, k, measure=c("fisher", "specificites", "binomial", "loglikelihood")) {
  wa <- matrix(0, nrow=max(N, n, K, k), ncol=length(measure));
  colnames(wa) <- measure;
  for (m in measure) {
    wa[,m] <- do.call(m, list(N, n, K, k));
  }
}

###########################################################################
##
## 3/
##
##
## The function for the various word attraction indicators
##
##
###########################################################################

##
 #
 #
 ##
fisher <- function(N, n, K, k) {
  stop("not implemented yet");
}

##
 #
 # Specificities
 #
 ##
specificities <- function(N, n, K, k) {
  length <- max(N, n, K, k);
  specif <- double(length=length);

  whiteDrawn <- k;
  allWhites <- K;
  allBlacks <- N-allWhites;
  drawn <- n;

  independance    <- (allWhites * drawn) / N;
  specif_negative <- whiteDrawn <  independance;
  specif_positive <- whiteDrawn >= independance;

  specif[specif_negative] <- phyper (
      whiteDrawn[specif_negative], allWhites[specif_negative], allBlacks[specif_negative], drawn
      );

  specif[specif_positive] <- phyper (
      whiteDrawn[specif_positive] - 1, allWhites[specif_positive], allBlacks[specif_positive], drawn
      );

  spelog <- double(length=length);
  spelog[spe < 0.5] <- log10(2*spe[spe < 0.5]);
  spelog[spe > 0.5] <- abs(log10(2 - 2*spe[spe > 0.5]));
  spelog[spe == 0.5] <- 0;
  spelog <- round(spelog, digits=4);
  return(spelog);
}

binomial <- function(N, n, K, k) {
  stop("not implemented yet");
}

##
 #
 # Log-likelihood function
 #
 # Stefan Evert notation of the argument:
 #
 #        --------------------------------
 #        |        | Col 1 | Col 2 |  T  |
 #        --------------------------------
 #        | Row 1  | $O11  | $O12  | $R1 |
 #        |        | $E11  | $E12  |     |
 #        --------------------------------
 #        | Row 2  | $O21  | $O22  | $R2 |
 #        |        | $E21  | $E22  |     |
 #        --------------------------------
 #        | Totals | $C1   | $C2   | $N  |
 #        --------------------------------
 #        
 #        N   = total words in corpus (or subcorpus or restriction, but they are not implemented yet)
 #        C1  = frequency of the collocate in the whole corpus
 #        C2  = frequency of words that aren't the collocate in the corpus
 #        R1  = total words in window
 #        R2  = total words outside of window
 #        O11 = how many of collocate there are in the window 
 #        O12 = how many words other than the collocate there are in the window (calculated from row total)
 #        O21 = how many of collocate there are outside the window
 #        O22 = how many words other than the collocate there are outside the window
 #        E11 = expected values (proportion of collocate that would belong in window if collocate were spread evenly)
 #        E12 =     "    "      (proportion of collocate that would belong outside window if collocate were spread evenly)
 #        E21 =     "    "      (proportion of other words that would belong in window if collocate were spread evenly)
 #        E22 =     "    "      (proportion of other words that would belong outside window if collocate were spread evenly)
 ##
loglikelihood <- function(N, n, K, k) {
  N <- N;
  C1 <- K;
  C2 <- N-K;
  R1 <- n;
  R2 <- N-n;
  O11 <- k;
  O12 <- n-k;
  O21 <- K-k;
  O22 <- C2 - O12;
  E11 = R1 * C1 / N;
  E12 = R1 * C2 / N;
  E21 = R2 * C1 / N;
  E22 = R2 * C2 / N;

#my $log_likelihood =
#  '2*( ((%O11% > 0) ? %O11% * log(%O11% / %E11%) : 0) +
#       ((%O12% > 0) ? %O12% * log(%O12% / %E12%) : 0) +
#       ((%O21% > 0) ? %O21% * log(%O21% / %E21%) : 0) +
#       ((%O22% > 0) ? %O22% * log(%O22% / %E22%) : 0)
#     )';

  measured <- sign(O11 - E11) * 2 * (
      ifelse(O11 > 0, O11 * log(O11 / E11), 0) +
      ifelse(O12 > 0, O12 * log(O12 / E12), 0) +
      ifelse(O21 > 0, O21 * log(O21 / E21), 0) +
      ifelse(O22 > 0, O22 * log(O22 / E22), 0)
      );
  return(measured);
}



write.mwa <- function(x, file, from=1, to=50, threshold=NULL, types=NULL, parts=NULL) {
  if(!class(x) == "specificities") stop("x must be of class specificities");
  if(is.null(file)) stop("file cannot be null");
  if (is.null(types)) types <- attr(x, "types")
  if (is.null(parts)) parts <- attr(x, "parts")
  printable <- .get.printable(x, from, to, threshold, types, parts);
  for (i in 1:length(printable)) {
      write.csv(printable[[i]], file=paste(file, "_", i, ".csv", sep=""));
  }
}


print.mwa <- function(x, from=1, to=50, threshold=NULL, types=NULL, parts=NULL, file="", append=FALSE, ...) {
  if (is.null(types)) types <- attr(x, "types")
  if (is.null(parts)) parts <- attr(x, "parts")

  printable <- .get.printable(x, from, to, threshold, types, parts);

  msg <- paste("Printing association measure for", length(printable), "part(s);");
  if (!is.null(threshold)) {
    msg <- paste(msg, "threshold:", threshold, "\n");
  } else {
    msg <- paste(msg, "from:", from, "to:", to, "\n");
  }
  cat(msg, file=file, append=append);

  cat(paste("Corpus size:", attr(x, "N"), "\n"), file=file, append=append);
  cat(sprintf("-----------------------------------------------------\n"), file=file, append=append);
  cat(sprintf("%-20s | %8s | %8s | %8s \n", "word", "specif.", "sub freq", "tot freq"), file=file, append=append);
  cat(sprintf("-----------------------------------------------------\n"), file=file, append=append);
  for (i_part in 1:length(printable)) {
    part <- printable[[i_part]];
    cat(".....................................................\n", file=file, append=append);
    cat(paste("Part name:", attr(part, "Part name"), "\n"), file=file, append=append);
    cat(paste("Part size:", attr(part, "Part size"), "tokens.", "\n"), file=file, append=append);
    cat(paste("Positive specificities:", sum(part[,2] > 0), "\n"), file=file, append=append);
    cat(paste("Negative specificities:", sum(part[,2] < 0), "\n"), file=file, append=append);
    if (nrow(part) > 0) {
      for (i_word in 1:nrow(part)) {
        word_name <- part[i_word, 1];
        word_specif <- part[i_word, 2];
        word_sub_freq <- part[i_word, 3];
        word_tot_freq <- part[i_word, 4];
        cat(sprintf("%-20s | %8.2f | %8.0f | %8.0f \n", word_name, word_specif, word_sub_freq, word_tot_freq), file=file, append=append);
      }
    }
  }
}


writeAsXML.specificities <-
function(x, file, from=1, to=100, threshold=NULL, types=NULL, parts=NULL) {
  if(!class(x) == "specificities") stop("x must be of class specificities");
  if(is.null(file)) stop("file cannot be null");
  if (is.null(types)) types <- attr(x, "types")
  if (is.null(parts)) parts <- attr(x, "parts")
  printable <- .get.printable(x, from, to, threshold, types, parts);
  .saveDataFrameAsXML(as.data.frame(printable), file);
}

.get.printable <-
function(x, from, to, threshold, types=NULL, parts=NULL ) {

  if(!class(x) == "specificities") stop("x must be of class specificities");

  if (is.null(threshold)) {
    if (is.null(from) | is.null(to)) {
      stop("either 'threshold' or 'from' and 'to' options must be given");
    }
    if ((!is.numeric(from)) | (!is.numeric(to))) {
      stop("both 'from' and 'to' must be numeric");
    }
    if (from >= to) {
      stop("'to' must be greater than 'from'");
    }
  } else {
    if (!is.numeric(threshold)) {
      stop("threshold must be numeric");
    }
    if (threshold <= 0) {
      stop("threshold must be greater than 0");
    }
  }
  if (is.null(types)) types <- 1:nrow(x);
  if (is.null(parts)) parts <- 1:ncol(x);

  freqtable <- attr(x, "frequency.table");
  part_names <- colnames(x);
  types <- rownames(x);
  part_lengths <- colSums(freqtable);
  form_freqs <- rowSums(freqtable);

  printable <- list();

  for (i_part in 1:length(parts)) {
    spe_part <- x[ , part_names[i_part] ];
    fre_part <- freqtable[ , parts[i_part] ];
    i_sorted <- order(spe_part, decreasing=TRUE);
    if (!is.null(threshold)) {
      i_selected_words <- which(abs(spe_part) > threshold);
      i_sorted <- i_sorted [ i_sorted %in% i_selected_words ];
	} else {
      i_min <- which.min(spe_part[i_sorted][spe_part[i_sorted] > 0]); # index of min positive specificities
	  if (length(i_min) == 0) {
		  i_min <- 0
	  }
      if (from <= i_min) {
        ii_positive <- from:min(to, i_min);
      } else {
        ii_positive <- numeric(0);
      }
      len <- length(i_sorted)+1;
      if ((len-from) > i_min) {
        ii_negative <- len-(to:from);
        ii_negative <- ii_negative[ii_negative > i_min];
      } else {
        ii_negative <- numeric(0);
      }
      i_sorted <- i_sorted[c(ii_positive, ii_negative)];
    }
    nbr_form <- length(i_sorted);
    p <- data.frame("Type"=character(nbr_form), "Specificites"=numeric(nbr_form), "Sub frequency"=integer(nbr_form), "Total frequency"=integer(nbr_form), stringsAsFactors=F);
	attr(p, "Part name") <- part_names[i_part];
    attr(p, "Part size") <- part_lengths[parts[i_part]];
    if (nbr_form > 0) {
      p[, 1] <- types[i_sorted];
      p[, 2] <- spe_part[i_sorted];
      p[, 3] <- fre_part[i_sorted];
      p[, 4] <- form_freqs[i_sorted];
    }
    printable <- append(printable, list(p));
  }
  return(printable);
}

# .get.printable <-
# function(x, frequency.table) {
#   if (any(dim(frequency.table) != dim(x))) {
#     stop("specificities and frequency table must have same dimensions");
#   }
# 
#   part_names <- rownames(x);
#   part_sizes <- rowSums(frequency.table);
#   types <- colnames(x);
#   form_freqs <- colSums(frequency.table);
# 
#   printable <- list();
# 
#   for (i_part in 1:nrow(x)) {
#     spe_part <- x[ i_part, ];
#     fre_part <- frequency.table[ i_part, ];
# 
#     i_sorted <- order(spe_part, decreasing=TRUE);
#     nbr_form <- length(i_sorted);
#     p <- data.frame(
#         "Type"=character(nbr_form),
#         "Specificites"=numeric(nbr_form),
#         "Sub frequency"=integer(nbr_form),
#         "Total frequency"=integer(nbr_form),
#         stringsAsFactors=F
#         );
#     attr(p, "Part name") <- part_names[i_part];
#     attr(p, "Part size") <- part_sizes[i_part];
#     if (nbr_form > 0) {
#       p[, 1] <- types[i_sorted];
#       p[, 2] <- spe_part[i_sorted];
#       p[, 3] <- fre_part[i_sorted];
#       p[, 4] <- form_freqs[i_sorted];
#     }
#     printable <- append(printable, list(p));
#   }
#   return(printable);
# }

# specificities.lexicon <-
# function(lexicon, sublexicon) {
#   spe <- specificities.lexicon.probabilities(lexicon, sublexicon);
#   spelog <- matrix(0, nrow=nrow(spe), ncol=ncol(spe));
#   spelog[spe < 0.5] <- log10(spe[spe < 0.5]);
#   spelog[spe > 0.5] <- abs(log10(1 - spe[spe > 0.5]));
#   spelog[spe == 0.5] <- 0;
#   spelog[is.infinite(spe)] <- 0;
#   spelog <- round(spelog, digits=4);
#   rownames(spelog) <- rownames(spe);
#   colnames(spelog) <- colnames(spe);
#   #class(spelog) <- "specificities";
#   #attr(spelog, "l.t") <- spe;
#   return(spelog);
# }
# 
# specificities.lexicon.probabilities <-
# function(lexicon, sublexicon) {
# 
#   if (!is.numeric(lexicon)) stop("The lexicon must contain numeric values.");
#   if (!is.numeric(sublexicon)) stop("The sublexicon must contain numeric values.");
#   if (is.null(names(lexicon))) stop("The lexicon must contain names.");
#   if (is.null(names(sublexicon))) stop("The sub lexicon must contain names.");
# 
#   if (! all(names(sublexicon) %in% names(lexicon)))
#     stop(
#         paste(
#           "Some requested types of the sublexicon are not known in the lexicon: ",
#           paste(names(sublexicon)[! (names(sublexicon) %in% names(lexicon))], collapse=" ")
#           )
#         ); 
# 
#   F <- sum(lexicon);
#   f <- sum(sublexicon);
# 
#   # complementary.lexicon <- c(lexicon[names(sublexicon)] - sublexicon, lexicon[!names(lexicon) %in% names(sublexicon)]);
# 
#   if (F < f) {
#     stop("The lexicon cannot be smaller than the sublexicon");
#   }
# 
#   whiteDrawn <- numeric(length(lexicon)); # The frequencies observed in this part for each type.
#   names(whiteDrawn) <- names(lexicon);
#   whiteDrawn[names(sublexicon)] <- sublexicon;
#   white <- lexicon; # The total frequencies in the corpus for each type.
#   black <- F-white;          # The total complement frequency in the corpus for each type.
#   drawn <- f;     # The number of tokens in the part.
# 
#   # print(whiteDrawn);
#   # print(white);
#   # print(black);
#   # print(drawn);
# 
#   independance    <- (white * drawn) / F;         # The theoretic frequency of each type.
# 
#   specif_negative <- whiteDrawn <  independance;  # index of observed frequencies below the theoretic frequencies.
#   specif_positive <- whiteDrawn >= independance;  # index of observed frequencies above the theoretic frequencies.
# 
#   specif <- double(length(lexicon));
# 
#   specif[specif_negative] <-     phyper (
#       whiteDrawn[specif_negative],     white[specif_negative], black[specif_negative], drawn
#       );
# 
#   specif[specif_positive] <- phyper (
#       whiteDrawn[specif_positive] - 1, white[specif_positive], black[specif_positive], drawn
#       );
# 
#   names(specif) <- names(lexicon);
#   
#   return(specif);
# }
# 
# #print.specificities(x, line=20, part=1, form=NULL, ...) {
# #  if (all(is.null(line, part))) {
# #    stop("either a line or a part must be specified");
# #  }
# #  if (all(!is.null(line, part))) {
# #    stop("only a line or a part must be specified");
# #  }
# #}
