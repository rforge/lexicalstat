\name{wam.loglikelihood}
\alias{wam.loglikelihood}
\title{
Calculate word association according to log likelihood.
}
\description{
Calculate word association according to log likelihood.
}
\usage{
wam.loglikelihood(N, n, K, k, p.value = TRUE, two.sided = FALSE)
}
\arguments{
  \item{N}{
the number of tokens in the corpus
}
  \item{n}{
the number of tokens in the subcorpus
}
  \item{K}{
the number of occurrences of the form under scrutiny in the corpus
}
  \item{k}{
the number of occurrences of the form under scrutiny in the subcorpus
}
  \item{p.value}{
%%     ~~Describe \code{p.value} here~~
}
  \item{two.sided}{
%%     ~~Describe \code{two.sided} here~~
}
}
\details{
\verb{
}
See \code{\link{wam}}

}
\value{
 A vector of numeric. 
 See \code{\link{wam}}
}
\references{
DUNNING, Ted, 1993. « Accurate methods for the statistics of surprise and
coincidence ». Dans : Computational linguistics. 19/1. MIT Press, pp. 61-74
}
\author{
Bernard Desgraupes
Sylvain Loiseau
}

\seealso{
See Also as \code{\link{wam}}
}
\examples{
}
