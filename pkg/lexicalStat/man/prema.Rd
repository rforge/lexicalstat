\name{prema}
\alias{prema}
\docType{data}
\title{
  Lexical table of corpus Prematures
}
\description{
  This lexical table contains 1015 short piece of text (rows) and 1829 lemma (columns). Each texts is redacted by nurces about premature baby. See also the data set \code{\link{prema.info}}
}
\usage{data(prema)}
\format{
  The format is a numeric matrix of 1015 rows and 1829 columns.
}

\source{
   See. Habert B. (2009) \emph{Construire des bases de donn\'ees pour le français}, Ophrys, Paris.
}

\examples{
data(prema);
data(prema.info);

by.proninf <- rowsum(prema, prema.info$proninf);
specificites(by.proninf, parts=c("MAUV", "TB"));

}
\keyword{datasets}
