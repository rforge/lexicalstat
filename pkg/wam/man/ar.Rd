\name{ar}
\alias{ar}
\docType{data}
\title{
Data for testing word association measures.
}
\description{
The data are presented in Kenneth Ward Church, Patrick Hanks (1990) \emph{Computational Linguistics}, 16/1, pages 22-29.
}
\usage{data(ar)}
\format{
  A data frame with 4 observations on the following 6 variables:
  \describe{
    \item{\code{N}}{a numeric vector} The corpus size
    \item{\code{word1}}{a character vector} the first wordform
    \item{\code{n}}{a numeric vector} the discourses sizes
    \item{\code{word2}}{a character vector} the second wordform
    \item{\code{K}}{a numeric vector} The total frequency of the word under scrutiny
    \item{\code{k}}{a numeric vector} The subfrequency of the 6 words in the 10 discourses.
  }
}
\source{
Kenneth Ward Church, Patrick Hanks (1990) \emph{Computational Linguistics}, 16/1, pages 22-29.
}
\examples{
data(ar)
attach(ar)
wam.ar(N, n, K, k)
detach(ar)
}
\keyword{datasets}
