\name{make.list}
\alias{make.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Turn a contingency table into a data frame of four column: N, n, K, k.
}
\description{
Turn a contingency table into a data frame of four column: N, n, K, k.
}
\usage{
make.list(contingency)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{contingency}{
the contingency table
}
}
\value{
Return a data frame with one row and four columns:, N, n, K, k
}
\author{
Sylvain Loiseau
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(happen)
df <- make.list(happen)
attach(df)
wam.collostruction(N, n, K, k)
detach(df)
}
