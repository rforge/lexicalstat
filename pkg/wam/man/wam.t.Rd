\name{wam.t}
\alias{wam.t}
\title{
Calculate word association according to the T test.
}
\description{
Calculate word association according to the T test.
}
\usage{
wam.t(N, n, K, k)
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
}
\details{
\verb{
 t-score =  prob(X=chemistry, Y=physics) - ( prob(X=chemistry) prob(Y=physics) )  
            ----------------------------------------------------------------------
                         sqrt((1/T) prob(X=chemistry, Y=physics))
}
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

wam.t(100, 20, 10, 4)

data(robespierre)
attach(robespierre)
wam(N, n, K, k, measure = wam.t, types, parts)
detach(robespierre)
}
