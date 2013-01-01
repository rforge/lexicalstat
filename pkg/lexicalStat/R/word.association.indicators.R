############################################################
##
##
## Functions for computing word attraction indicators
##
## All functions take four arguments: N, n, K, k
##
##
############################################################

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

  # used data.frame to recycle vector length toward the same length.
  # otherwise, phyper below may produce NA (if whites=1 => 1[FALSE, TRUE, TRUE, ...])
  recycled <- data.frame(whiteDrawn=k, whites=K, blacks=N-K, drawn=n);

  independance    <- (recycled$whites * recycled$drawn) / N;
  specif_positive <- recycled$whiteDrawn >= independance;

  specif <- double(length=nrow(recycled));

  pos_arg <- recycled[specif_positive, ];
  specif[specif_positive] <- phyper (
      pos_arg$whiteDrawn, pos_arg$whites, pos_arg$blacks, pos_arg$drawn
      );

  neg_arg <- recycled[!specif_positive, ];
  specif[!specif_positive] <- phyper (
      neg_arg$whiteDrawn, neg_arg$whites, neg_arg$blacks, neg_arg$drawn
      );

  spelog <- double(length=nrow(recycled));
  spelog[specif < 0.5] <- log10(2*specif[specif < 0.5]);
  spelog[specif > 0.5] <- abs(log10(2 - 2*specif[specif > 0.5]));
  spelog[specif == 0.5] <- 0;
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

