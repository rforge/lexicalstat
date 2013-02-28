\docType{methods}
\name{freq}
\alias{freq}
\alias{freq,FrequencyListDataFrame,character-method}
\title{Get the frequencies of given linguistic type.}
\arguments{
  \item{corpus}{Any \code{\link{FrequencyList}} concrete
  subclass.}

  \item{types}{The linguistic types for which the frequency
  is requested.}
}
\section{Methods}{
\describe{
\item{\code{signature(corpus = "FrequencyListDataFrame", types = "character")}}{
Return the frequency (between \code{0} and \code{N(corpus)}) for each requested types.
}
}}
\value{
  A named numeric vector of integer.
}
\description{
  Get the frequencies of given linguistic type.
}
\examples{
data(dickensFrequencyList)
freq(dickensFrequencyList, c("the", "is", "it"));
}

\keyword{methods}