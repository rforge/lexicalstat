\name{happen}
\alias{happen}
\docType{data}
\title{
A contingency table lexem ~ construction
}
\description{

A 2*2 contingency table containing the frequency of lexem in the slot of a construction :
\tabular{lrr}{
                    \tab accident\tab !accident\cr
N waiting to happen \tab       14\tab        21\cr
!N waiting to happen\tab     8606\tab  10197659\cr
}
}
\usage{data(happen)}
\format{
  A 2*2 numeric matrix
}
\source{
Stefanowitsch A. \& Gries St. Th. 2003 "Collostructions: Investigating the interaction of words and constructions", \emph{International Journal of Corpus Linguistics}, 8/2, 209-234, p. 219.
}
\examples{
data(happen)
happen <- make.list(happen)
attach(happen)
wam.collostruction(N, n, K, k)
detach(happen)
}
\keyword{datasets}
