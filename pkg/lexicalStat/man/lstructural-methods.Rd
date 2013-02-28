\docType{methods}
\name{lstructural}
\alias{lstructural}
\alias{lstructural-method}
\alias{lstructural,TabulatedDataFrame-method}

\title{Get the list of the names of the structural attribute.}
\arguments{
  \item{corpus}{Any \code{\link{Tabulated}} concrete
  subclass.}
}
\section{Methods}{
\describe{

\item{\code{signature(corpus = "TabulatedDataFrame")}}{
%%  ~~describe this method here~~
}
}}
\value{
  A character vector
}
\description{
  Get the list of the names of the structural attribute.
}
\examples{
data(dickensTabulated)
lstructural(dickensTabulated);
}

\keyword{methods}