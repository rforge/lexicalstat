\docType{methods}
\name{types}
\alias{types}
\alias{types,FrequencyListDataFrame-method}
\alias{types,LexicalTableSparseMatrix-method}
\alias{types,WordAssociation-method}
\title{List of the types of a corpus.}
\usage{
\S4method{types}{LexicalTableSparseMatrix}(corpus)
\S4method{types}{FrequencyListDataFrame}(corpus)
}
\section{Methods}{
\describe{

\item{\code{signature(corpus = "FrequencyListDataFrame")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(corpus = "LexicalTableSparseMatrix")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(corpus = "WordAssociation")}}{
%%  ~~describe this method here~~
}
}}
\arguments{
  \item{corpus}{Any \code{\link{CorpusAsFrequencies}}
  concrete subclass.}
}
\value{
  A character vector containing the list of the linguistic
  types (unique tokens) in the corpus.
}
\description{
  List of the types of a corpus.
}
\examples{
data(dickensFrequencyList)
types(dickensFrequencyList);
}
\seealso{
  \code{\link{ntype}}
}

\keyword{methods}