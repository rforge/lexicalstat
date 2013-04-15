\name{K}
\docType{methods}
\alias{K}
\alias{K-methods}
\alias{K,WordAssociation-method}
\title{Total frequency of forms}
\description{
Total frequency of forms in a \code{\link{WordAssociation}} object
}
\section{Methods}{
\describe{
\item{\code{signature(obj = "WordAssociation")}}{
Any word association object (see \code{\link{wam}}).
}
}}
\examples{
data(robespierre)
attach(robespierre)
x <- wam(N, n, K, k, measure = c(wam.chisq, wam.loglikelihood), types, parts)
detach(robespierre)
K(x)

}

\keyword{methods}
