\name{wam.chisq}
\alias{wam.chisq}
\title{
Calculate word association according to the chi square test.
}
\description{
Calculate word association according to the chi square test.
}
\usage{
wam.chisq(N, n, K, k, yates.correction = TRUE, p.value = TRUE, two.sided = FALSE)
}
\arguments{
  \item{N}{
the number of tokens in the corpus
}
  \item{n}{
the number of tokens in the subcorpus
}
  \item{K}{
the number of occurrences of the form under scrutiny in the corpus
}
  \item{k}{
the number of occurrences of the form under scrutiny in the subcorpus
}

  \item{yates.correction}{

}
  \item{p.value}{

}
  \item{two.sided}{

}
}
\details{
See \code{\link{wam}}

}
\value{
 A vector of numeric. 
 See \code{\link{wam}}
}
\references{
CHURCH, K., GALE, W., HANKS, P. & HINDLE, D.,  1991. « Using Statistics
in Lexical Analysis ». In : ZERNIK, U. (ed.), Lexical Acquisition.
Hillsdale, Lawrence Erlbaum Ass., pp. 115--164
}
\author{
Bernard Desgraupes
Sylvain Loiseau
}

\seealso{
See Also as \code{\link{wam}}
}
\examples{

wam.chisq(100, 20, 10, 4)

data(robespierre)
attach(robespierre)
wam(N, n, K, k, measure = "chisq", types, parts)
detach(robespierre)

}
