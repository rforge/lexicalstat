\name{robespierre}
\alias{robespierre}
\docType{data}
\title{Lexical table from Robespierre's discourses}
\description{
  A lexical table containing the frequencies of 5 words in 10 Robespierre public
  discourses (between november 1793 and july 1794).
}
\usage{data(robespierre)}
\format{
  A matrix
}
\details{
  The last column of the table give the total frequency of all the other forms
in each of these discourses.
}
\source{

  Lafon P. (1980) Sur la variabilit \' e de la fr \' e quence des formes dans un corpus, Mots, 1, pp. 127--165.

}
\references{

  Lafon P. (1980) Sur la variabilit \' e de la fr \' e quence des formes dans un corpus, Mots, 1, pp. 127--165.

}
\examples{
data(robespierre)

## See graphic in Lafon, 1980 : 140
t <- rowSums(robespierre)["D9"];     # size of the part
T <- sum(robespierre);             # size of the corpus
f <- colSums(robespierre)["peuple"]; # total frequency of "peuple"
p <- dhyper(1:15, f, T-f, t)
plot(p, type="h", main="Probabilities of various frequencies of 'peuple' in discourse 'D9'", xlab="Frequency of 'peuple'", ylab="Prob(k)");

specificites(robespierre, parts="D9");

}
\keyword{datasets}
