\name{print-methods}
\docType{methods}
\alias{print-methods}
\alias{print,ANY-method}
\alias{print,WordAssociation-method}
\title{Pretty-print of the pieces of information in a \code{\link{WordAssociation}} object}
\description{
Pretty-print of the word attractions measures in a \code{\link{WordAssociation}} object. The lexical types
are grouped by subcorpora and, in each subcorpora, print the types sorted according to the requested word association measure.
}
\section{Methods}{
\describe{
\item{\code{signature(x = "WordAssociation")}}{
Pretty-print a WordAssociation object
}
}}
		  
\arguments{
  \item{from}{numeric. The first word to be printed in each corpora, according to the sorting of the words (see below sort.by).}
  \item{to}{numeric. The last word to be printed in each corpora, according to the sorting of the words (see below sort.by).}
  \item{threshold}{numeric. Do no print words whose association is less than the given threshold.}
  \item{types}{character. Print only the given types.}
  \item{parts}{character. Print only the given subcorpora.}
  \item{sort.by=indicator.name(x)[1]}{character. The name of one of the word association indicator available in the  \code{\link{WordAssociation}} object for sorting the lexical types in each subcorpora.}
  \item{file}{character. If no empty, print the table in the given file}
  \item{append}{logical. If the \code{file} argument is no empty, select erasing file or adding the table at the end of the file, preserving existing content.}
}
\value{
  Nothing
}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
