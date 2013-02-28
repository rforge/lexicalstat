\docType{methods}
\name{hapax}
\alias{hapax}
\alias{hapax,FrequencyListDataFrame-method}
\title{The list of linguistic types occurring only once.}
\description{
  The list of linguistic types occurring only once.
}
\arguments{
  \item{corpus}{Any \code{\link{FrequencyList}} concrete
  subclass.}
}
\section{Methods}{
\describe{

\item{\code{signature(corpus = "FrequencyListDataFrame")}}{
Give the list of types whose frequency is equal to 1.
}
}}
\value{
  A character vector.
}
\examples{
data(dickensFrequencyList)
hapax(dickensFrequencyList);
}

\keyword{methods}
