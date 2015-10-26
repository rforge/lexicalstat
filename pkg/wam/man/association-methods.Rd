\name{association-methods}
\docType{methods}
\alias{association-methods}
\alias{association,WordAssociation-method}
\title{ Get the association strengh }
\description{
 Get the association strengh for all association computed in this \code{\link{WordAssociation}} object
}
\section{Methods}{
\describe{

\item{\code{signature(obj = "WordAssociation")}}{
The association strength as a vector of numeric
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

