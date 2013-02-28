\name{fullText}
\alias{fullText}
\title{
\code{\link{FullText}} constructor
}
\description{
Create a \code{\link{FullText}} corpus thanks to a list of tokens
}
\usage{
  fullText(l, depth=1)
}
\arguments{
  \item{l}{A named list of character vectors. The name are the part names; the character vectors are the tokens of each part}
  \item{depth}{the depth of recursivity of the list. Currently only 1 is allowed.}
}
\seealso{
  \code{\link{FullText}}
}
\value{
  A \code{\link{FullText}} corpus.
} 
