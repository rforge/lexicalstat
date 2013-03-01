\name{line2paragraph}
\alias{line2paragraph}
\title{Convert a vector of lines into a vector of paragraphs}
\usage{
  line2paragraph(lines)
}
\description{
  A paragraph is delimited by an empty line). Consecutive
  lines are concatenated (an " " is added)
}
\arguments{
  \item{lines}{A character vector of lines belonging to the same or to different paragraphs.}
}
\examples{
fileName <- system.file(c("inst",  "exempleData"), "small.paragraphs", package="lexicalStat")
lines <- readLines(fileName)
line2paragraph(lines);
}

