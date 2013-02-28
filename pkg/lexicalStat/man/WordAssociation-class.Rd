\name{WordAssociation}
\alias{WordAssociation}
\alias{WordAssociation-class}
\title{The result of word association measure.}
\description{
Compute and hold word association measure indicators.

See \code{\link{wam}} for constructing WordAssociation object thanks to high level
objects such as \code{\link{Corpus}} and \code{\link{wordAssociation}} for the low-level
constructor.
}
\details{
  For each element the following pieces of information are available :
 \itemize{
   \item K
   \item k
   \item N
   \item n
   \item type
   \item part
   \item one or several computed association measure indicators
   }

See the functions \code{\link{N}}, \code{\link{n}}, \code{\link{K}} and \code{\link{k}}
for retrieving these pieces of information.
   
}
\author{
  Sylvain Loiseau
}
\seealso{
\code{\link{wam}}, \code{\link{wordAssociation}}
}

