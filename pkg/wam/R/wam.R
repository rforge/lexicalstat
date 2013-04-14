# ===========================================================================
# File: "wam.R"
#                        Created: 2013-04-05 14:55:29
#              Last modification: 2013-04-08 11:59:45
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


wam.specificities <- function(N, n, K, k, method="log") {
# mode
  mo <- floor((n+1)*(K+1)/(N+2));

# cdf (or its log)
  islog <- (method %in% c("log","logscale"))
    cdk <- ifelse(k <= mo, phyper(k, K, N-K, n, log.p=islog), phyper(k-1, K, N-K, n, log.p=islog, lower.tail=FALSE));
  specif <- double(length(k));

  if (method == "base") {
    specif <- ifelse(k <= mo, -cdk, cdk);
  } else { 
# cumulative probability for the mode (or its log)
    cdmo <- phyper(mo, K, N-K, n, log.p=islog);
    if (method == "gap") {
      specif <- ifelse(k <= mo, -abs(cdmo-cdk), abs(cdmo-cdk));
    } else if (method == "log") {
      specif <- ifelse(k <= mo, -abs(cdmo-cdk), abs(cdmo-cdk));
    } else if (method == "scale") {
      specif <- ifelse(k <= mo, -abs(cdmo-cdk)/cdmo, abs(cdmo-cdk)/cdmo);
    } else if (method == "logscale") {
      specif <- ifelse(k <= mo, -abs((cdmo-cdk)/cdk) , abs((cdmo-cdk)/cdk));
    } 
  }

  return(specif);
}

wam.z <- function(N, n, K, k, yates.correction=FALSE) {
  C1 <- K;
  R1 <- n;
  O11 <- k;
  E11 = R1 * C1 / N;

  diff <- O11 - E11;
  if (yates.correction) {
    diff <- diff - 0.5;
  }
  return(diff / sqrt(E11));
}

wam.t <- function(N, n, K, k) {
  cont <- .make.contingency(N, n, K, k)

    C1 <- K;
  R1 <- n;
  O11 <- k;
  E11 = R1 * C1 / N;

  t <- (O11 - E11) / sqrt(O11);
  return ( - pnorm(t, 0, 1, lower.tail=FALSE, log.p=TRUE) / log(10) );
}

wam.chisq <- function(N, n, K, k, yates.correction=TRUE, p.value=TRUE, two.sided=FALSE) {
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

  if (yates.correction) {
    chi_squared = N * (abs(O11 * O22 - O12 * O21) - N / 2) ** 2 / (R1 * R2 * C1 * C2);
  } else {
    chi_squared = N * (O11 * O22 - O12 * O21) ** 2 / (R1 * R2 * C1 * C2);
  }

  if (! two.sided) {
    if (! p.value) stop("p.value mandatory with 'two.sided=FALSE'");
    z <- ifelse(O11 >= E11, sqrt(chi_squared), -sqrt(chi_squared));
    return(pnorm(z, 0, 1, lower.tail=FALSE));
  }

  if (p.value) {
    return (pchisq(chi_squared, 1, "upper") );
  } else {
    return(chi_squared);
  }

}

# PMI = log2 ( prob(X=chemistry, Y=physics) / ( prob(X=chemistry) prob(Y=physics))  
#
#If PMI = 0, then the two variables are independent.
#   PMI > 0, then the two variables are related.
#    wam.mi <- function(N, n, K, k) {
#    }

wam.fisher <- function(N, n, K, k) {
    c <-.make.contingency(N, n, K, k);
    return(fisher.test(c)$p.value);
}

wam.g <- function(N, n, K, k) {
}

.make.contingency <- function(N, n, K, k) {
    c <- matrix(c(k, K-k, n-k, (N-K)-(n-k)), 2, 2, dimnames=list(c("n", "!n"), c("k", "!k")));
    return(c);
}

