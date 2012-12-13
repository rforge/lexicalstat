\name{sentences}
\alias{sentences}
\docType{data}
\title{Text of four sentences}
\description{
   List of four elements, each one representing a sentence as a vector of lemma.
}
\usage{data(sentences)}

\format{
A list of character vector.
}

\source{
First four sentences of G. Deleuze and F. Guattari (1972) \emph{Anti-Oedipe}, Minuit, Paris.
}

\examples{
data(sentences)
list2lexical.table(sentences);
}
\keyword{datasets}
