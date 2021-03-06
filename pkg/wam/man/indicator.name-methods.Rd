\name{indicator.name-methods}
\docType{methods}
\alias{indicator.name-methods}
\alias{indicator.name}
\alias{indicator.name,WordAssociation-method}
\title{ The names of the statistical indicators used in a \code{\link{WordAssociation}} object}
\description{
The list of the indicator names.
}
\section{Methods}{
\describe{

\item{\code{signature(obj = "WordAssociation")}}{
A word association object.
}
}}
\examples{
data(robespierre)
attach(robespierre)
x <- wam(N, n, K, k, measure = c("chisq", "loglikelihood", "specificities"), types, parts)
indicator.name(x)
detach(robespierre)
}
\keyword{methods}

