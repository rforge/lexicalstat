\docType{methods}
\name{lpositional}
\alias{lpositional}
\alias{lpositional-method}
\alias{lpositional,TabulatedDataFrame-method}
\title{Get the list of the names of the positional attribute.}
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
  Get the list of the names of the positional attribute.
}
\examples{
data(dickensTabulated)
lpositional(dickensTabulated);
}

\keyword{methods}