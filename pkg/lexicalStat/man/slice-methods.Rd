\docType{methods}
\name{slice}
\alias{slice}
\alias{slice,FullTextList,ANY-method}
\alias{slice,FullTextList-method}
\alias{slice,Tabulated-method}
\alias{slice,Tabulated,ANY-method}
\alias{slice,Tabulated,numeric,character-method}
\title{Reshape a corpus according to the positions of tokens (beginning or end of corpus parts)}
\arguments{
  \item{corpus}{Any \code{\link{CorpusAsTokens}} concrete
  subclass.}
  \item{n}{The number of slices the parts or regions have to be devided into}
  \item{structural.attribute}{For a Tabulated argument, give the region for slicing}
}
\section{Methods}{
\describe{

\item{\code{signature(corpus = "FullTextList")}}{
Each part of the corpus is devided into \code{n} groups of equals length.
The groups of same index are joined (all first group..., each second groups...)
and a new \code{\link{FullText}} corpus containing \code{n} parts is created.
}
\item{\code{signature(corpus = "FullTextList")}}{
Each region of the corpus, as given by \code{structural.attribute}, is devided into
\code{n} groups of equals length. The groups of same index are joined
(all first group..., each second groups...) and a new \code{\link{Tabulated}} corpus,
containing one structural attribute, \code{Slice}, with \code{n} regions, is created.
}
}}
\value{
  A \code{\link{FullText}} object where the parts contains
  tokens at specific position (i.e. in specific slices) in
  the input corpus parts.
}
\description{
  Reshape a corpus according to the positions of tokens
  (beginning or end of corpus parts)
}
\examples{
data(dickensFullText)
print(dickensFullText)
slice(dickensFullText, 3);

data(dickensTabulated)
slice(dickensTabulated, 3, "sentence");
}

\keyword{methods}