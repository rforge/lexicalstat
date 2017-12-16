\name{wam.ar}
\alias{wam.ar}
\title{
Calculate word association measure
}
\description{
The association ratio is a 
}
\usage{
wam.ar(N, n, K, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{K}{
%%     ~~Describe \code{K} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Kenneth Ward Church, Patrick Hanks (1990) \textit{Computational Linguistics}, 16/1, pages 22-29.
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
