\name{robespierre}
\alias{robespierre}
\docType{data}
\title{
A tiny corpus ready for word association measures.
}
\description{

A tiny corpus containing with frequencies of 6 words in 10 discourses par Robespierre (between november 1793 and july 1794)

This data frame study 6 words in 10 discourses ; it contains therefore 60 rows.

The words : \code{de} \code{ennemi} \code{others} \code{patrie} \code{peuple}
\code{republique}.

}
\usage{data(robespierre)}
\format{
  A data frame with 60 observations on the following 6 variables.
  \describe{
    \item{\code{N}}{a numeric vector} The corpus size
    \item{\code{n}}{a numeric vector} the discourses sizes
    \item{\code{K}}{a numeric vector} The total frequency of the word under scrutiny
    \item{\code{k}}{a numeric vector} The subfrequency of the 6 words in the 10 discourses.
    \item{\code{types}}{The words represented by this line}
    \item{\code{parts}}{The part concerned with this line}
  }
}
\source{
LAFON, Pierre, 1980. « Sur la variabilité de la fréquence des formes dans un corpus ». In : Mots. 1. pp. 127--165.
}
\examples{
data(robespierre)
attach(robespierre)
wam(N, n, K, k, measure = "chisq", types, parts)
detach(robespierre)
}
\keyword{datasets}
