\name{frequencyList}
\alias{frequencyList}
\alias{frequencyList.data.frame}
\alias{frequencyList.table}
\alias{frequencyList.numeric}
\title{Construct a \code{\link{FrequencyList}}}
\description{
  Construct a \code{\link{FrequencyList}}
}
\usage{
  frequencyList(x)
 \method{frequencyList}{data.frame}(x)
 \method{frequencyList}{numeric}(x)
 \method{frequencyList}{table}(x)
}
\arguments{
  \item{x}{the data with the types and their frequency: either a data.frame with two columns, "type" and
  "frequency" (with that names); or a a named numeric vector; or a table object (such as created with
  \code{\link{table}})}
}
\seealso{
  \code{\link{FrequencyList}}
}
\value{
  \code{\link{FrequencyList}}
} 
