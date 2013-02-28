\docType{methods}
\name{as.FullText}
\alias{as.FullText}
\alias{as.FullText-method}
\alias{as.FullText,Tabulated,character,character-method}
\alias{as.FullText,FullText,ANY,ANY-method}
\title{Convert a corpus into a \code{\link{FullText}} corpus}
\description{
  Only \code{\link{Tabulated}} corpus can be converted into \code{\link{FullText}}.
}
\section{Methods}{
\describe{

\item{\code{signature(x = "FullText", positional = "ANY", structural = "ANY")}}{
The input object is returned.
}

\item{\code{signature(x = "Tabulated", positional = "character", structural = "character")}}{
The parts of the \code{FullText} corpus group all the token (as given by the positional attribute argument)
with the same value in the structural attribute given(i.e. belonging to the same region in this
structural attribute). The parts are named after the id of the corresponding region.
Tokens that are outside any region in this structural attribute (hence having a value of "-1"
in this structural attribute) are grouped in a part with the name "-1". While the other parts of
the resulting \code{FullText} group consecutive tokens, the part with the name "-1" may
group non-consecutive token of the input corpus.
}
}}
\value{
  A \code{\link{FullText}} corpus
}
\seealso{
  \code{\link{Corpus}} for an overview of the possible
  conversions between corpus classes
}
\examples{
data(dickensTabulated)
as.FullText(dickensTabulated, "word", "sentence");
}

\keyword{methods}