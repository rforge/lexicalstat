\name{parts-methods}
\docType{methods}
\alias{parts}
\alias{parts-methods}
\alias{parts,WordAssociation-method}
\title{The part of the corpus each entry belong to in a \code{\link{WordAssociation}} object}
\description{
The part of the corpus each entry belong to in a \code{\link{WordAssociation}} object
}
\section{Methods}{
\describe{

\item{\code{signature(obj = "WordAssociation")}}{
%%  ~~describe this method here~~
}
}}
\examples{
data(robespierre)
attach(robespierre)
x <- wam(N, n, K, k, measure = c("chisq", "loglikelihood"), types, parts)
parts(x)
detach(robespierre)
}

\keyword{methods}
