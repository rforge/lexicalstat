\name{N}
\docType{methods}
\alias{N}
\alias{N-methods}
\alias{N,FrequencyListDataFrame-method}
\alias{N,FullTextList-method}
\alias{N,LexicalTableSparseMatrix-method}
\alias{N,TabulatedDataFrame-method}
\alias{N,WordAssociation-method}
\title{Number of tokens of a corpus}
\description{
Get the number of tokens of a corpus.
}

\usage{
\S4method{N}{FullTextList}(corpus)
\S4method{N}{LexicalTableSparseMatrix}(corpus)
\S4method{N}{TabulatedDataFrame}(corpus)
\S4method{N}{FrequencyListDataFrame}(corpus)
}
\arguments{
  \item{corpus}{A \code{\link{Corpus}} object}
}

\section{Methods}{
\describe{

\item{\code{signature(corpus = "FrequencyListDataFrame")}}{
}

\item{\code{signature(corpus = "FullTextList")}}{
}

\item{\code{signature(corpus = "LexicalTableSparseMatrix")}}{
}

\item{\code{signature(corpus = "TabulatedDataFrame")}}{
}

\item{\code{signature(corpus = "WordAssociation")}}{
}
}}
\value{
  The number of tokens in the corpus.
}
\examples{
data(dickensFrequencyList)
N(dickensFrequencyList);
}
\seealso{
  \code{\link{ntypes}} and \code{\link{type}}
}
\keyword{methods}
