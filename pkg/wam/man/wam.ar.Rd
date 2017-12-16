\name{wam-functions}
\alias{wam.ar}
\alias{wam.fisher}
\alias{wam.chisq}
\alias{wam.frequency}
\alias{wam.jaccard}
\alias{wam.loglikelihood}
\alias{wam.MI}
\alias{wam.specificities}
\alias{wam.t}
\alias{wam.z}
\title{
Calculate word association measure according to various fomula
}
\description{
Word association measures give an information about the tendency of two
words to co-occurr with greater (or lesser) than chance frequency.
}

\usage{
wam.ar(N, n, K, k)
wam.fisher(N, n, K, k)
wam.chisq(N, n, K, k)
wam.frequency(N, n, K, k)
wam.jaccard(N, n, K, k)
wam.loglikelihood(N, n, K, k)
wam.MI(N, n, K, k)
wam.specificities(N, n, K, k)
wam.t(N, n, K, k)
wam.z(N, n, K, k)
}
\arguments{
  \item{N}{
The total number of occurrences in the corpus
}
  \item{n}{
The frequency of word 1
}
  \item{K}{
The frequency of word 2
}
  \item{k}{
The frequency of the cooccurrence or word 1 and word 2.
}
}
\details{


}
\value{
A numeric values. All function are vectorized.
}
\references{

For association ratio : Kenneth Ward Church, Patrick Hanks (1990) \textit{Computational Linguistics}, 16/1, pages 22-29.
}
\author{
Sylvain Loiseau
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (N, n, K, k) 
{
    c <- make.contingency(N, n, K, k)
    return(fisher.test(c)$p.value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
