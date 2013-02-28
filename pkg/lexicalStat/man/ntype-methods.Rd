\docType{methods}
\name{ntype}
\alias{ntype}
\alias{ntype-method}
\alias{ntype,FrequencyListDataFrame-method}
\alias{ntype,LexicalTableSparseMatrix-method}
\title{Number of types of a corpus.}
\usage{
\S4method{ntype}{FrequencyListDataFrame}(corpus)
\S4method{ntype}{LexicalTableSparseMatrix}(corpus)
}
\arguments{
  \item{corpus}{Any \code{\link{CorpusAsFrequencies}}
  concrete subclass.}
}
\section{Methods}{
\describe{

\item{\code{signature(corpus = "FrequencyListDataFrame")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(corpus = "LexicalTableSparseMatrix")}}{
%%  ~~describe this method here~~
}
}}
\value{
  The number of linguistic types (unique tokens) in the
  corpus.
}
\description{
  Number of types of a corpus.
}
\examples{
data(dickensFrequencyList)
ntype(dickensFrequencyList);
}
\seealso{
  \code{\link{types}}
}

\keyword{methods}