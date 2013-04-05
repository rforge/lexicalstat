# ===========================================================================
# File: "wam.specificities.R"
#                        Created: 2013-04-05 14:55:29
#              Last modification: 2013-04-05 14:55:29
# Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
#          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
# This file is part of the wam project.
# ===========================================================================

#
# word association measure defined in : LAFON, Pierre, 1980. « Sur la
# variabilité de la fréquence des formes dans un corpus », /Mots/, 1, pp.
# 127--165
#
# http://www.persee.fr/web/revues/home/prescript/article/mots_0243-6450_1980_num_1_1_1008
#

wam.specificities <- function(N, n, K, k, method="log") {
  len <- max(length(N), length(n), length(K), length(k));
  N <- rep(N, length.out=len);
  n <- rep(n, length.out=len);
  K <- rep(K, length.out=len);
  k <- rep(k, length.out=len);

  # the number of black balls in the urn.
  B <- N-K;

  # the mode
  mo <- floor((n+1)*(B+1)/(N+2));
  #mode <- ((recycled$whites + 1) * (recycled$drawn + 1)) / (N + 2);

  specif <- double(len);
  if (method == "log") { # use log.p and lower.tail
    specif <- ifelse(k <= mo, -abs(phyper(k, K, N-K, n, log.p=TRUE)), abs(phyper(k, K, N-K, n, log.p=TRUE, lower.tail=FALSE)));
  } else { # compute cdk
    cdk <- ifelse(k <= mo, -phyper(k, K, N-K, n), 1-phyper(k-1, K, N-K, n));
    if (method == "base") {
      specif <- cdk;
    } else { # compute cdmo (cumulative probability for the mode)
      cdmo <- phyper(mo, B, N-B, n);
      if (method == "gap") {
	specif <- ifelse(k <= mo, -abs(cdmo-cdk), abs(cdmo-cdk));
      } else if (method == "logscale") {
	specif <- ifelse(k <= mo, -abs((cdmo-cdk)/cdk) , abs((cdmo-cdk)/cdk));
      } else if (method == "scale") {
	specif <- ifelse(k <= mo, -abs(cdmo-cdk)/cdmo, abs(cdmo-cdk)/cdmo);
      }
    }
  }
  return(specif);
}

