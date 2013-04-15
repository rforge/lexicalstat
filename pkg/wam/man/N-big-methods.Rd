\name{N}
\docType{methods}
\alias{N}
\alias{N-methods}
\alias{N,WordAssociation-method}
\title{Number of tokens of a corpus}
\description{
Number of tokens in its corpus for each forms in a \code{\link{WordAssociation}} object.
}

\usage{
\S4method{N}{WordAssociation}(obj)
}
\arguments{
  \item{obj}{A \code{\link{obj}} object}
}

\section{Methods}{
\describe{

\item{\code{signature(obj = "WordAssociation")}}{
Any word association object (see \code{\link{wam}}).
}
}}
\value{
  The number of tokens in the corpus.
}
\examples{

data(robespierre)
attach(robespierre)
x <- wam(N, n, K, k, measure = c(wam.chisq, wam.loglikelihood), types, parts)
detach(robespierre)
N(x)

}
\seealso{
  \code{\link{types}}
}
\keyword{methods}
