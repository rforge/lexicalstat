\name{wam.specificities}
\alias{wam.specificities}
\title{
Calculate word association according to the specificities method.
}
\description{
Calculate word association according to the specificities method; i.e.
(hypergeometric distribution)
}
\usage{
wam.specificities(N, n, K, k, method = "log")
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
  \item{method}{
  the method used:
\itemize{
  \item base
  \item log
  \item logscale
  \item gap
  \item scale
}
}
}
\details{
\verb{
}
See \code{\link{wam}}

}
\value{
 A vector of numeric. 
 See \code{\link{wam}}
}
\references{
LAFON, Pierre, 1980. « Sur la variabilité de la fréquence des formes
dans un corpus ». Dans : Mots. 1. pp. 127--165.

http://www.persee.fr/web/revues/home/prescript/article/mots_0243-6450_1980_num_1_1_1008

}
\author{
Bernard Desgraupes
Sylvain Loiseau
}

\seealso{
See Also as \code{\link{wam}}
}
\examples{
}
