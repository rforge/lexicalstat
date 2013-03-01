\docType{methods}
\name{tactique.subcorpus}
\alias{tactique.subcorpus}
\alias{tactique.subcorpus,FullTextList,ANY-method}
\alias{tactique.subcorpus,FullTextList-method}
\title{Select tokens according to their position (beginning or end of corpus parts)}
\arguments{
  \item{corpus}{Any \code{\link{CorpusAsTokens}} concrete
  subclass.}
}
\section{Methods}{
\describe{

\item{\code{signature(corpus = "FullTextList")}}{
%%  ~~describe this method here~~
}
}}
\value{
  A \code{\link{FullText}} object where the parts are
  grouping of "slices" (tokens at specific position in the
  input corpus parts) containing a given word.
}
\description{
  Given a corpus and a partition in parts, extract a
  subcorpus containing all the tokens found in a given
  "slice" in the parts and containing a given linguistic
  type.
}
\examples{
# The first half containing 'the' of all parts in the corpus
data(dickensFullText)
tactique.subcorpus(dickensFullText, 1, 2, "the");
}

\keyword{methods}