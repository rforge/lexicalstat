\docType{methods}
\name{as.LexicalTable}
\alias{as.LexicalTable}
\alias{as.LexicalTable,FullText-method}
\alias{as.LexicalTable,LexicalTable-method}
\alias{as.LexicalTable,Tabulated-method}
\alias{as.LexicalTable,Tabulated,character,character-method}
\title{Convert a corpus into a \code{\link{LexicalTable}} corpus}
\usage{
\S4method{as.LexicalTable}{FullText}(x)
\S4method{as.LexicalTable}{LexicalTable}(x)
\S4method{as.LexicalTable}{Tabulated}(x, positional, structural)
}

\arguments{
  \item{x}{a \code{\link{Corpus}}}
  \item{positional}{a positional attribute for selecting the linguistic types to be counted}
  \item{structural}{a structural attribute for dividing the tokens into parts}
}
\section{Methods}{
\describe{

\item{\code{signature(x = "FullText")}}{

}

\item{\code{signature(x = "LexicalTable")}}{

}

\item{\code{signature(x = "Tabulated")}}{

}
}}
\value{
  A \code{\link{LexicalTable}} corpus
}
\description{
  Convert a corpus into a \code{\link{LexicalTable}} corpus
}
\examples{
data(dickensTabulated)
as.LexicalTable(dickensTabulated, "word", "sentence");

data(dickensFullText)
as.LexicalTable(dickensFullText);
}
\seealso{
  \code{\link{Corpus}} for an overview of the possible
  conversions between corpus classes
}

\keyword{methods}