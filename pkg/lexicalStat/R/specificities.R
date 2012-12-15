
## 
 # ------------------------------------------------------------------------
 # 
 # "specificities" --
 # 
 # lexicaltable = a matrix of nrow types and ncol parts
 # 
 # ------------------------------------------------------------------------
 ##
specificities <-
function(lexicaltable, types=NULL, parts=NULL) {
  spe <- specificities.probabilities(lexicaltable, types, parts);
  #dim(spe);
  spelog <- matrix(0, nrow=nrow(spe), ncol=ncol(spe));
  spelog[spe < 0.5] <- log10(2*spe[spe < 0.5]);
  spelog[spe > 0.5] <- abs(log10(2 - 2*spe[spe > 0.5]));
  spelog[spe == 0.5] <- 0;
  spelog <- round(spelog, digits=4);
  rownames(spelog) <- rownames(spe);
  colnames(spelog) <- colnames(spe);
  class(spelog) <- "specificities";
  attr(spelog, "frequency.table") <- lexicaltable;
  attr(spelog, "types") <- types;
  attr(spelog, "parts") <- parts;
  attr(spelog, "corpussize") <- attr(spe, "F");

  return(spelog);
}

 
## 
 # ------------------------------------------------------------------------
 # 
 # "specificities.probabilities" --
 # 
 # lexicaltable = a matrix of nrow types and ncol parts
 # 
 # ------------------------------------------------------------------------
 ##
specificities.probabilities <-
function(lexicaltable, types=NULL, parts=NULL) {

  #if (!is.numeric(lexicaltable)) stop("The lexical table must contain numeric values.");

  partMargin <- colSums(lexicaltable); # or "F" (the total frequency of all the types).
  typeMargin <- rowSums(lexicaltable); # or "T" (the size of the parts).
  if (any(typeMargin == 0)) {
    stop("Row without any occurrence");
  }
  if (any(partMargin == 0)) {
    stop("Column without any occurrence");
  }

  F <- sum(partMargin);             # The grand total (number of tokens in the corpus).

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

  specif <- matrix(0.0, nrow=nrow(lexicaltable), ncol=ncol(lexicaltable));

  for(i in 1:ncol(lexicaltable)) {
	  # We proceed the whole lexical table by col (i.e. by part).
     whiteDrawn <- lexicaltable[,i];  # The frequencies observed in this part for each type.
     allWhites <- typeMargin;         # The total frequencies in the corpus for each type.
     allBlacks <- F-allWhites;        # The total complement frequency in the corpus for each type.
     drawn <- partMargin[i];          # The total number of occurrences in the part.

     independance    <- (allWhites * drawn) / F;     # The theoretic frequency of each type.
     specif_negative <- whiteDrawn <  independance;  # index of observed frequencies below the theoretic frequencies.
     specif_positive <- whiteDrawn >= independance;  # index of observed frequencies above the theoretic frequencies.

     specif[specif_negative, i] <- phyper (
         whiteDrawn[specif_negative], allWhites[specif_negative], allBlacks[specif_negative], drawn
         );

     specif[specif_positive, i] <- phyper (
         whiteDrawn[specif_positive] - 1, allWhites[specif_positive], allBlacks[specif_positive], drawn
         );
  }

  dimnames(specif) <- dimnames(lexicaltable);

  attr(specif, "F") <- F;
  return(specif);
}


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


#N <- urne_size;
#C1 <- white;
#C2 <- N-C1;
#R1 <- drawn;
#R2 <- N-R2;
#
#O11 <- white_found:
#O12 <- R1 - O11;
#O21 <- C1 - O11;
#O22 <- C2 - O12;
#
#E11 = R1 * C1 / N;
#E12 = R1 * C2 / N;
#E21 = R2 * C1 / N;
#E22 = R2 * C2 / N;
#
#sign(O11 - E11) * 2 * (
#    ifelse(O11 > 0, O11 * log(O11 / E11), 0) +
#    ifelse(O12 > 0, O12 * log(O12 / E12), 0) +
#    ifelse(O21 > 0, O21 * log(O21 / E21), 0) +
#    ifelse(O22 > 0, O22 * log(O22 / E22), 0)
#    );
#

specificities.lexicon <-
function(lexicon, sublexicon) {

  if (!is.numeric(lexicon)) stop("lexicon must be numeric vector");
  if (!is.numeric(sublexicon)) stop("sub lexicon must be numeric vector");

  if (length(lexicon) == 0) stop("lexicon cannot have 0 form");
  if (length(sublexicon) == 0) stop("sub lexicon cannot have 0 form");

  if (is.null(names(lexicon))) stop("lexicon must have name");
  if (is.null(names(sublexicon))) stop("sublexicon must have name");

  if (any("" == names(lexicon))) stop("empty string cannot be a lexicon entry");
  if (any("" == names(sublexicon))) stop("empty string cannot be a sublexicon entry");

  if (! all(names(sublexicon) %in% names(lexicon))) {
    i <- names(sublexicon) %in% names(lexicon);
    stop(
	paste(
	  sum(! i),
	  "types of the sublexicon not found in the lexicon: ",
	  paste(names(sublexicon)[!i], collapse=", ")
	  )
	);
  }

  sub <- numeric(length(lexicon));
  names(sub) <- names(lexicon);
  sub[names(sublexicon)] <- sublexicon;

  complementary.lexicon <- lexicon;
  complementary.lexicon[names(sublexicon)] <- lexicon[names(sublexicon)] - sublexicon;

  # lexicon;
  if (any(complementary.lexicon < 0)) stop("type cannot be more frequent in the sublexicon than in the lexicon");

  lexical.table <- matrix(c(sub, complementary.lexicon), nrow=2, byrow=T);

  colnames(lexical.table) <- names(lexicon);
  rownames(lexical.table) <- c("sublexicon", "complementary");
  # for debuging, instead of 
  #return(specificities(lexical.table, parts=1));
  sss <- specificities(lexical.table);
  return(sss);
}


write.specificities <-
function(x, file, from=1, to=50, threshold=NULL, types=NULL, parts=NULL) {
  if(!class(x) == "specificities") stop("x must be of class specificities");
  if(is.null(file)) stop("file cannot be null");
  if (is.null(types)) types <- attr(x, "types")
  if (is.null(parts)) parts <- attr(x, "parts")
  printable <- .get.printable(x, from, to, threshold, types, parts);
  for (i in 1:length(printable)) {
      write.csv(printable[[i]], file=paste(file, "_", i, ".csv", sep=""));
  }
}


print.specificities <-
function(x, from=1, to=50, threshold=NULL, types=NULL, parts=NULL, file="", append=FALSE, ...) {
  if (is.null(types)) types <- attr(x, "types")
  if (is.null(parts)) parts <- attr(x, "parts")

  printable <- .get.printable(x, from, to, threshold, types, parts);

  msg <- paste("Printing specificities for", length(printable), "part(s);");
  if (!is.null(threshold)) {
    msg <- paste(msg, "threshold:", threshold, "\n");
  } else {
    msg <- paste(msg, "from:", from, "to:", to, "\n");
  }
  cat(msg, file=file, append=append);

  cat(paste("Corpus size:", attr(x, "corpussize"), "\n"), file=file, append=append);
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

# .get.printable <-
# function(x, frequency.table) {
#   if (any(dim(frequency.table) != dim(x))) {
#     stop("specificities and frequency table must have same dimensions");
#   }
# 
#   part_names <- rownames(x);
#   part_sizes <- rowSums(frequency.table);
#   forms <- colnames(x);
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
#       p[, 1] <- forms[i_sorted];
#       p[, 2] <- spe_part[i_sorted];
#       p[, 3] <- fre_part[i_sorted];
#       p[, 4] <- form_freqs[i_sorted];
#     }
#     printable <- append(printable, list(p));
#   }
#   return(printable);
# }

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
  forms <- rownames(x);
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
      p[, 1] <- forms[i_sorted];
      p[, 2] <- spe_part[i_sorted];
      p[, 3] <- fre_part[i_sorted];
      p[, 4] <- form_freqs[i_sorted];
    }
    printable <- append(printable, list(p));
  }
  return(printable);
}

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
