\name{CorpusAsFrequencies}
\alias{CorpusAsFrequencies}
\alias{CorpusAsFrequencies-class}
\title{The parent of classes representing corpus as a list frequencies of types}
\description{
  This virtual class is the root hierarchy of all classes
  representing corpora with frequency count of linguistic
  types rather than with the list of actual phenomena.
}
\details{
  Subclass of \code{CorpusAsFrequencies} must implement the
  generic methods \code{\link{ntype}} and
  \code{\link{types}}.
}
\author{
  Sylvain Loiseau
}
\seealso{
  \code{\link{Corpus}} (for an overview of the available
  classes) and the subclasses \code{\link{FrequencyList}}
  and \code{\link{LexicalTable}}
}

