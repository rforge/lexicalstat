\docType{methods}
\name{kwic}
\alias{kwic}
\alias{kwic,FullTextList,ANY-method}
\alias{kwic,TabulatedDataFrame,ANY-method}
\alias{kwic,FullTextList-method}
\alias{kwic,TabulatedDataFrame-method}
\title{Print context of a token in the "key word in context" (KWIC) form.}
\arguments{
  \item{corpus}{Any \code{\link{CorpusAsTokens}} concrete
  subclass.}

  \item{pattern}{A regexp for selecting tokens to be used
  as keyword.}

  \item{left}{Number of characters on the left}

  \item{right}{Number of characters to be printed on the
  right}
}
\section{Methods}{
\describe{

\item{\code{signature(corpus = "FullTextList")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(corpus = "TabulatedDataFrame")}}{
%%  ~~describe this method here~~
}
}}

\value{
  A character vector of kwic.
}
\description{
  Print context of a token in the "key word in context"
  (KWIC) form.
}
\examples{
data(dickensFullText)
kwic(dickensFullText, "the");
}

\keyword{methods}