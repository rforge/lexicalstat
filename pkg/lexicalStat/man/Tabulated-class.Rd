\name{Tabulated}
\alias{Tabulated}
\alias{Tabulated-class}
\title{The parent of classes representing corpus as a list of tokens representend as tuples}
\description{
  This virtual class is a subclass of
  \code{\link{CorpusAsTokens}}. Corpus of type
  \code{\link{Tabulated}} give several representation for
  each tokens (such as part of speech, inflected form,
  lemma) and each token may belong to named range of
  different kinds (such as a sentence, a paragraph, a
  chapter...)
}
\details{
  Each character representation of tokens is called a
  \emph{positional attribute}. Each kind of token sequences
  is called a \emph{structural attribute}. This corpus
  representation is inspired by corpus representation in
  the CWB Workbench corpus.

  Subclass of \code{FrequencyList} must implement the
  generic methods \code{\link{subfreq}}.
}
\author{
  Sylvain Loiseau
}
\seealso{
  its superclass \code{\link{CorpusAsTokens}}; its sibling
  \code{\link{FullText}}; one of its implementation:
  \code{\link{TabulatedDataFrame}}. \code{\link{Corpus}} for
  an overview of the available classes.

  \url{http://cwb.sourceforge.net}
}

