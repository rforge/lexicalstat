\docType{methods}
\name{as.Tabulated}
\alias{as.Tabulated}
\alias{as.Tabulated,FullText-method}
\alias{as.Tabulated,Tabulated-method}
\title{Convert a corpus into a \code{\link{Tabulated}} corpus}
\description{
  Actually, only \code{\link{FullText}} corpus can be
  converted into \code{\link{Tabulated}}
}
\arguments{
  \item{corpus}{a \code{\link{Corpus}}}
}
\section{Methods}{
\describe{

\item{\code{signature(corpus = "FullText")}}{
A \code{\link{Tabulated}} corpus with only one positional attribute (named "word") and
only one structural attribute (named "part") is created.
}

\item{\code{signature(corpus = "Tabulated")}}{
The input corpus is given unchanged.
}
}}

\value{
  A \code{\link{Tabulated}} corpus
}
\seealso{
  \code{\link{Corpus}} for an overview of the possible
  conversions between corpus classes
}
\examples{
data(dickensFullText)
as.Tabulated(dickensFullText);
}

\keyword{methods}