\name{k}
\docType{methods}
\alias{k}
\alias{k-methods}
\alias{k,WordAssociation-method}
\title{Subfrequencies of forms in a \code{\link{WordAssociation}} object}
\description{
Subfrequencies of forms in a \code{\link{WordAssociation}} object.
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
k(x)

}

\keyword{methods}

