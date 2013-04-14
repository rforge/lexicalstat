\name{wam.MI}
\alias{wam.binomial}
\alias{wam.fisher}
\alias{wam.loglikelihood}
\alias{wam.MI}
\alias{wam.specificities}
\title{Word attraction measure based on Mutual information}
\usage{
  wam.MI(N, n, K, k)

  wam.fisher(N, n, K, k)

  wam.specificities(N, n, K, k)

  wam.binomial(N, n, K, k)

  wam.loglikelihood(N, n, K, k)
}
\arguments{
  \item{N}{number of tokens in the corpus}

  \item{n}{number of tokens in the subcorpus}

  \item{K}{frequency of the form in the corpus}

  \item{k}{frequency of the form in the subcorpus}
}
\description{
  Word attraction measure based on Mutual information

  Word attraction measure based on the Fisher exact test

  Word attraction measure based on the hypergeometric
  distribution

  Word attraction measure based on the binomial
  distribution

  Word attraction measure based on the binomial
  distribution
}
\examples{
#wam.MI(10, 7, 5, 4);
#wam.fisher(10, 7, 5, 4);
wam.specificities(10, 7, 5, 4);
wam.binomial(10, 7, 5, 4);
wam.loglikelihood(10, 7, 5, 4);
}

