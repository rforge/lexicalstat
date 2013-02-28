\docType{methods}
\name{subfreq}
\alias{subfreq}
\alias{subfreq-methods}
\alias{subfreq,LexicalTableSparseMatrix,ANY,ANY-method}
\alias{subfreq,LexicalTable-method}
\title{Get the subfrequencies of given linguistic types in given types.}
\section{Methods}{
\describe{

\item{\code{signature(corpus = "LexicalTableSparseMatrix")}}{
%%  ~~describe this method here~~
}
}}
\arguments{
  \item{corpus}{Any \code{\link{LexicalTable}} concrete
  subclass.}

  \item{types}{The linguistic types for which the
  subfrequencies are requested.}

  \item{parts}{The parts for which the subfrequencies are
  requested.}
}
\value{
  A data.frame where columns = (Type, Part, Subrequency).
}
\description{
  Get the subfrequencies of given linguistic types in given
  types.
}
\examples{
data(dickensLexicalTable)
subfreq(dickensLexicalTable, "the", "3");
}

\keyword{methods}