\docType{methods}
\name{types}
\alias{types}
\alias{types,WordAssociation-method}
\title{List of the types of a corpus.}
\usage{
\S4method{types}{WordAssociation}(obj)
}
\section{Methods}{
\describe{

\item{\code{signature(obj = "WordAssociation")}}{
%%  ~~describe this method here~~
}
}}
\arguments{
  \item{obj}{\code{\link{WordAssociation}} subclass.}
}
\value{
  A character vector containing the list of the linguistic
  types corresponding to the computed association measure.
}
\description{
  List of the types of a corpus.
}
\examples{
data(robespierre)
attach(robespierre)
x <- wam(N, n, K, k, measure = c(wam.chisq, wam.loglikelihood), types, parts)
detach(robespierre)
types(x)

}
\seealso{
  \code{\link{parts}}
}

\keyword{methods}
