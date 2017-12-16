\name{make.contingency}
\alias{make.contingency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Turn a flat list representation into a contingency table representation
}
\description{
Turn a flat list representation into a contingency table representation
}
\usage{
make.contingency(N, n, K, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
The total number of occurrences in the corpus
}
  \item{n}{
The number of occurrence in the subcorpora
}
  \item{K}{
The total frequency of the form in the corpus
}
  \item{k}{
The subfrequency of the form in the subcorpora
}
}
\value{
Return a 2*2 contingency table :
  \tabular{lll}{
       \tab K       \tab ¬K          \cr
    n  \tab k       \tab n-k         \cr
    ¬n \tab K-k     \tab (N-K)-(n-k) \cr
  }
}
\author{
Sylvain Loiseau
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(robespierre)
peuple_D4 <- robespierre[robespierre$types=="peuple" & robespierre$parts == "D4",]
peuple_D4
attach(peuple_D4)
make.contingency(N, n, K, k)
}
