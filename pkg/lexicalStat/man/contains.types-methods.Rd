\docType{methods}
\name{contains.types}
\alias{contains.types}
\alias{contains.types,FrequencyListDataFrame,character-method}
\title{Ask if a FrequencyList contains the given linguistic types.}
\arguments{
  \item{corpus}{Any \code{\link{FrequencyList}} concrete
  subclass.}

  \item{types}{The linguistic types.}
}
\section{Methods}{
\describe{

\item{\code{signature(corpus = "FrequencyListDataFrame", types = "character")}}{

}
}}
\value{
  A named logical vector.
}
\description{
  Ask if a FrequencyList contains the given linguistic
  types.
}
\examples{
data(dickensFrequencyList)
contains.types(dickensFrequencyList, c("the", "xxxx"));
}

\keyword{methods}