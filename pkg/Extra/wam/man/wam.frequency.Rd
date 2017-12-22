\name{wam.frequency}
\alias{wam.frequency}
\title{
Calculate word association according to the raw frequency of the word under scrutiny.
}
\description{
Calculate word association according to the raw frequency of the word under scrutiny
}
\usage{
wam.frequency(N, n, K, k, yates.correction = TRUE, p.value = TRUE, two.sided = FALSE)
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

}
\details{
See \code{\link{wam}}. The \code{k} argument is returned.

}
\value{
 A vector of numeric. 
 See \code{\link{wam}}
}
\references{
}
\author{
Bernard Desgraupes
Sylvain Loiseau
}

\seealso{
See Also as \code{\link{wam}}
}
\examples{

wam.frequency(100, 20, 10, 4)

}
