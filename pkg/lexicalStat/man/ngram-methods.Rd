\docType{methods}
\name{ngram}
\alias{ngram}
\alias{ngram,FullTextList,ANY-method}
\alias{ngram,FullTextList,ANY,ANY-method}
\alias{ngram,FullTextList-method}
\title{Create ngram (all the sequences of consecutive n tokens)}
\description{
  Create ngram (all the sequences of consecutive n tokens)
}
\arguments{
  \item{corpus}{Any \code{\link{CorpusAsTokens}} concrete subclass.}
  \item{n}{Number of consecutive tokens for creating ngrams.}
  \item{sep}{string for concatenating tokens of the ngrams.}
}
\value{
  A \code{\link{CorpusAsTokens}} object where the tokens
  are ngram of the input corpus.
}
\section{Methods}{
\describe{

\item{\code{signature(corpus = "FullTextList")}}{
%%  ~~describe this method here~~
}
}}
\examples{
data(dickensFullText)
ngram(dickensFullText);

data(dickensTabulated)
ngram(dickensTabulated);
}

\keyword{methods}