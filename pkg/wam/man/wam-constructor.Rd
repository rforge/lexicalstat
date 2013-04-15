\name{wam-constructor}
\alias{wam}
\title{Compute association measure and construct a \code{\link{WordAssociation}} object.}
\usage{
  wam(N, n, K, k,
    measure = wam.loglikelihood, types = NULL, parts = NULL, ...)
}
\arguments{
  \item{types}{graphical form of the linguistic types}

  \item{parts}{name of the subset of the corpus with which
  the attraction is measured}

  \item{N}{number of tokens in the corpus}

  \item{n}{number of tokens in the subcorpus}

  \item{K}{frequency of the type in the corpus}

  \item{k}{frequency of the type in the subcorpus}

  \item{measure}{function implementing statistical attraction measures. These functions must
  accept 'N', 'n', 'K' and 'k' arguments}

  \item{...}{argument provided to function "measure"}
}
\description{
  Compute association measure and construct a
  \code{\link{WordAssociation}} object.
}
\seealso{
  \code{\link{WordAssociation}}
}

\examples{

data(robespierre)
attach(robespierre)
wam(N, n, K, k, measure = c(wam.chisq, wam.loglikelihood), types, parts)
detach(robespierre)

}
