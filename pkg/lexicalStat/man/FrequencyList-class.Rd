\name{FrequencyList}
\alias{FrequencyList}
\alias{FrequencyList-class}
\title{The parent of classes representing corpus as a list of total frequencies of types}
\description{
This virtual class is a subclass of \code{\link{CorpusAsFrequencies}} and the root
hierarchy of all classes representing corpora as a list of types with their total frequencies.

Subclass of \code{FrequencyList} must implement the generic methods
\code{\link{freq}}, \code{\link{hapax}} and \code{\link{contains.types}}.
}
\author{
  Sylvain Loiseau
}
\seealso{
Its superclass \code{\link{CorpusAsFrequencies}}; its sibling \code{\link{LexicalTable}};
one of its implementation: \code{\link{FrequencyListDataFrame}}. \code{\link{Corpus}}
for an overview of the available classes.
}
