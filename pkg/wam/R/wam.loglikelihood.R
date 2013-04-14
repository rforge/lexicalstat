# ===========================================================================
# File: "wam.specificities.R"
#                        Created: 2013-04-05 14:55:29
#              Last modification: 2013-04-05 14:55:29
# Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
#          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
# This file is part of the wam project.
# ===========================================================================


#Dunning 1993 accurate method for the Statistics of Surprise and coincidence

#http://pioneer.chula.ac.th/~awirote/colloc/statmethod1.htm

wam.loglikelihood <- function(N, n, K, k, p.value=TRUE, two.sided=FALSE) {
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

  ll <- 2 * (
      ifelse(O11 > 0, O11 * log(O11 / E11), 0) +
      ifelse(O12 > 0, O12 * log(O12 / E12), 0) +
      ifelse(O21 > 0, O21 * log(O21 / E21), 0) +
      ifelse(O22 > 0, O22 * log(O22 / E22), 0)
      );

if (! two.sided) {
  if (! p.value) stop("p.value mandatory with 'two.sided=FALSE'");
  z <- ifelse(O11 >= E11, sqrt(ll), -sqrt(ll));
  return(pnorm(z, 0, 1, lower.tail=FALSE));
}

  if (p.value) {
    ll <- pchisq(ll, 1, lower.tail=FALSE);
  }
  return(ll);      
}

