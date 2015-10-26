\name{n-methods}
\docType{methods}
\alias{n}
\alias{n-methods}
\alias{n,WordAssociation-method}
\title{The size of the sub corpus in a word association list}
\description{
Number of tokens in its subcorpus for each forms in a \code{\link{WordAssociation}} object.
}
\section{Methods}{
\describe{
\item{\code{signature(obj = "WordAssociation")}}{
Any word association object (see \code{\link{wam}}).
}
}}
\seealso{
  \code{\link{N}}, \code{\link{K}}, \code{\link{k}}, and \code{\link{WordAssociation}} for
  the big picture.
}
\examples{
data(robespierre)
attach(robespierre)
x <- wam(N, n, K, k, measure = c("chisq", "loglikelihood"), types, parts)
n(x)
detach(robespierre)
}

\keyword{methods}
