\docType{methods}
\name{as.FrequencyList}
\alias{as.FrequencyList}
\alias{as.FrequencyList-methods}
\alias{as.FrequencyList,FrequencyList,ANY-method}
\alias{as.FrequencyList,FullText,ANY-method}
\alias{as.FrequencyList,LexicalTable,ANY-method}
\alias{as.FrequencyList,Tabulated,character-method}
\title{Convert a corpus into a \code{\link{FrequencyList}} corpus}
\description{
  Convert a corpus into a \code{\link{FrequencyList}}
  corpus
}
\arguments{
  \item{corpus}{a \code{\link{Corpus}}}
  \item{positional}{a positional attribute (for Tabulated corpus)}
}
\section{Methods}{
\describe{
\item{\code{signature(x = "FrequencyList", positional = "ANY")}}{
The input object is returned
}

\item{\code{signature(x = "FullText", positional = "ANY")}}{
The distribution of tokens into different parts is lost.
}

\item{\code{signature(x = "LexicalTable", positional = "ANY")}}{
The distribution of tokens into different parts is lost.
}

\item{\code{signature(x = "Tabulated", positional = "character")}}{
The frequency list contains occurrence for the type found in the given positional attribute.
}
}}
\value{
  A \code{\link{FrequencyList}} corpus
}
\seealso{
  \code{\link{Corpus}} for an overview of the possible
  conversions between corpus classes
}
\examples{
data(dickensTabulated)
as.FrequencyList(dickensTabulated, "word");

data(dickensFullText)
as.FrequencyList(dickensFullText);

data(dickensLexicalTable)
as.FrequencyList(dickensLexicalTable);
}

\keyword{methods}