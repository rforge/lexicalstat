\name{CorpusAsTokens}
\alias{CorpusAsTokens}
\alias{CorpusAsTokens-class}
\title{The parent of classes representing corpus as a list of tokens with their textual order}
\description{
  This virtual class is the root hierarchy of all classes
  representing corpora with each actual tokens, in their
  textual order
}
\details{
  Subclass of \code{CorpusAsTokens} must implement the
  generic methods \code{\link{ngram}}, \code{\link{kwic}},
  \code{\link{tactique.subcorpus}} and \code{\link{slice}}.
}
\author{
  Sylvain Loiseau
}
\seealso{
  \code{\link{Corpus}} (for an overview of the available
  classes) and the subclasses \code{\link{FullText}} and
  \code{\link{Tabulated}}
}

