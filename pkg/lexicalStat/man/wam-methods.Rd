\docType{methods}
\name{wam}
\alias{wam}
\alias{wam-methods}
\alias{wam,FrequencyList,FrequencyList-method}
\alias{wam,FullText,missing-method}
\alias{wam,FullText,numeric-method}
\alias{wam,Tabulated,missing-method}
\alias{wam,Tabulated,NULL-method}
\alias{wam,Tabulated,Tabulated-method}
\alias{wam,Tabulated,FullText,NULL-method}
\alias{wam,Tabulated,LexicalTable,ANY-method}
\title{Compute word association measure (wam) given a corpus object.}
\section{Methods}{
\describe{

\item{\code{signature(corpus = "FrequencyList", subcorpus = "FrequencyList")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(corpus = "FullText", subcorpus = "missing")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(corpus = "FullText", subcorpus = "NULL")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(corpus = "FullText", subcorpus = "numeric")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(corpus = "LexicalTable", subcorpus = "ANY")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(corpus = "Tabulated", subcorpus = "missing")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(corpus = "Tabulated", subcorpus = "NULL")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(corpus = "Tabulated", subcorpus = "Tabulated")}}{
%%  ~~describe this method here~~
}
}}
\arguments{
  \item{corpus}{a \code{\link{Corpus}}}

  \item{subcorpus}{a \code{\link{Corpus}} representing a
  subset of the \code{Corpus} argument. For the Corpus with
  part (\code{\link{LexicalTable}}, \code{\link{FullText}}),
  may be missing or NULL (in which case the attracted forms
  are measured for each part) or numeric vector giving the
  index of the parts of the corpus to be used as
  subcorpus.}
}
\value{
  Return an object \code{\link{WordAssociation}} This
  object can be printed with several options (see function
  \code{\link{print.WordAssociation}}).
}
\description{
  Compute word association measure (wam) given a corpus
  object.
}
\examples{
data(dickensFullText)
wam(dickensFullText, 1)
}
\seealso{
  \code{\link{WordAssociation}},
  \code{\link{print.WordAssociation}}
}

\keyword{methods}