# much often using S. Evert UCS package (perl/lib/HTest.pm).

##
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

wam.z <- function(N, n, K, k, yates.correction=FALSE) {
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

  diff <- O11 - E11;
  if (yates.correction) {
    diff <- diff - 0.5;
  }
  return(diff / sqrt(E11));
}

# Church et. al. (1991)
wam.t.test <- function() {
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

  t <- (O11 - E11) / sqrt(O11);
  # voir Evert's UCS, perl/lib/R.pm
  return ( - pnorm(t, 0, 1, lower.tail=FALSE, log=TRUE) / log(10));
}