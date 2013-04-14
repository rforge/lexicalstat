\name{wordAssociation}
\alias{wordAssociation}
\title{Compute association measure and construct a \code{\link{WordAssociation}} object.}
\usage{
  wordAssociation(N, n, K, k,
    measure = "wam.specificities", types, parts)
}
\arguments{
  \item{types}{graphical form of the linguistic types}

  \item{parts}{name of the subset of the corpus with which
  the attraction is measured}

  \item{N}{number of tokens in the corpus}

  \item{n}{number of tokens in the subcorpus}

  \item{K}{frequency of the type in the corpus}

  \item{k}{frequency of the type in the subcorpus}

  \item{measure}{names of the statistical attraction measures requested}
}
\description{
  Compute association measure and construct a
  \code{\link{WordAssociation}} object.
}
\seealso{
  \code{\link{WordAssociation}}
}

