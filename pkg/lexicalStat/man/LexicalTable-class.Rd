\name{LexicalTable}
\alias{LexicalTable}
\alias{LexicalTable-class}
\title{The parent of classes representing corpus as a table of subfrequencies of types in parts of a corpus}
\description{
  This virtual class is a subclass of
  \code{\link{CorpusAsFrequencies}} and the root hierarchy
  of all classes representing corpora as subfrequencides of
  tokens in parts.
}
\details{
  Subclass of \code{FrequencyList} must implement the
  generic methods \code{\link{subfreq}}.
}
\author{
  Sylvain Loiseau
}
\seealso{
  its superclass \code{\link{CorpusAsFrequencies}}; its
  sibling \code{\link{FrequencyList}}; one of its
  implementation: \code{\link{LexicalTableSparseMatrix}}.
  \code{\link{Corpus}} for an overview of the available
  classes.
}

