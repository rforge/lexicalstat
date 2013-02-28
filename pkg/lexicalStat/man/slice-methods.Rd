\docType{methods}
\name{slice}
\alias{slice}
\alias{slice,FullTextList,ANY-method}
\alias{slice,FullTextList-method}
\title{Reshape a corpus according to the positions of tokens (beginning or end of corpus parts)}
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
}

\keyword{methods}