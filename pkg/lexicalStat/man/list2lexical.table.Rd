\name{list2lexical.table}
\Rdversion{1.1}
\alias{list2lexical.table}

\title{
  Create a lexical table given a corpus
}
\description{
  Given a list of character vectors representing a corpus divided in sub-corpora, create a lexical table where lines represent the subcorpora and columns represent forms.
}
\usage{
list2lexical.table(l)
}

\arguments{
  \item{l}{
    A list of character vectors. The elements of the list represent a subdivision of the corpus into subcorpora. The vector in each list element contains the tokens.
}
}

\details{
  The empty string \code{""} are removed from the character vectors.
}

\value{
  A data frame representing a lexical table i.e. a contingency table. Each cell
  give the subfrequency of the given form in the given part of the corpus.
  Rownames and colnames of the data frame give part names and forms. If the
  list is not named, rownames are integer number starting at \code{0}. 
}

\author{
  Sylvain Loiseau
}

\examples{
  data(sentences);
  list2lexical.table(sentences);
}
