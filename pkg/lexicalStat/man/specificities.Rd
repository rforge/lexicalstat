\name{specificities}
\alias{specificities}

\title{ Compute specificity association score }
\description{

 Compute specificities association score, i.e. an indicator of the association
 between tokens and sub-corpus, given a lexical table

}
\usage{
specificities(lexicaltable, types=NULL, parts=NULL)
}

\arguments{

  \item{lexicaltable}{ a complete lexical table, i.e. a numeric matrix where
  each line is a token and each column is a part of a corpus. Each cell gives the
  frequency of the given token in the corresponding part of the corpus. }

  \item{types}{ Indicate for which rows the specificity index must be computed.
  If \code{NULL}, the specificity index is computed for every form. If
  \code{types} is a character vector, it indicates the names of the rows of
  \code{lexicaltable} to be used (an error is thrown if \code{lexicaltable} has no row
  names). If it is an integer vector, it indicates the indices of the rows to
  use.}

  \item{parts}{ Indicate for which columns the specificity index must be computed.
  If \code{NULL}, the specificity index is computed for every part. If
  \code{parts} is a character vector, it indicates the names of the columns of
  \code{lexicaltable} to be used (an error is thrown if \code{lexicaltable} has no
  column names). If it is an integer vector, it indicates the indices of the
  columns to use.}

}

\value{
  
The function returns a matrix of \code{nrow * ncol} where \code{nrow} is
the number of types and \code{ncol} is the number of parts specified by the
\code{types} or \code{parts} arguments). Each cell gives the frequency
index.

}

\references{
  Lafon P. (1980), \emph{Sur la variabilité de la fréquence des formes dans un corpus},
  Mots, Vol. 1, Issue 1, pp. 127--165.
}

\author{Sylvain Loiseau}

\seealso{\code{\link{specificities.lexicon}}}

\examples{
data(robespierre);
specificities(robespierre);
specificities(robespierre, types="peuple", parts=c("D9", "D10"));
}
