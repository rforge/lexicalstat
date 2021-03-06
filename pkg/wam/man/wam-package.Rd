\name{wam-package}
\alias{wam-intro}
\alias{wam-package}

\title{Word Association Measures}
\description{
  \tabular{ll}{
    Version: \tab \Sexpr[stage=render]{library(utils); packageVersion("wam")}\cr
    Date: \tab \Sexpr[stage=build]{format(Sys.time(),"\%Y-\%m-\%d")}\cr
    License: \tab \Sexpr[stage=build]{library(utils); packageDescription("wam")$License} \cr
  }
}

\details{
  The package \code{wam} give an implementation of several word association measures used
  in corpus linguistics.

In all functions, the arguments are :

\itemize{
  \item N = the number of tokens in the corpus
  \item n = the number of tokens in the subcorpus
  \item K = the number of occurrences of the form under scrutiny in the corpus
  \item k = the number of occurrences of the form under scrutiny in the subcorpus
}

This can be easily turn into the "contingency table" representation used in some
presentation (according to Stefan Evert UCS documentation) :

\verb{
        --------------------------------------
        |              | word | ¬ word |  T  |
        --------------------------------------
        | subcorpus    | O11  | O12    | R1  |
        |              | E11  | E12    |     |
        --------------------------------------
        | ¬ subcorpus  | O21  | O22    | R2  |
        |              | E21  | E22    |     |
        --------------------------------------
        | Totals       | C1   | C2     | N   |
        --------------------------------------
}

where :
\itemize{
\item         N   = total words in corpus (or subcorpus or restriction, but they are not implemented yet)
\item         C1  = frequency of the collocate in the whole corpus
\item         C2  = frequency of words that aren't the collocate in the corpus
\item         R1  = total words in window
\item         R2  = total words outside of window
\item         O11 = how many of collocate there are in the window 
\item         O12 = how many words other than the collocate there are in the window (calculated from row total)
\item         O21 = how many of collocate there are outside the window
\item         O22 = how many words other than the collocate there are outside the window
\item         E11 = expected values (proportion of collocate that would belong in window if collocate were spread evenly)
\item         E12 =     "    "      (proportion of collocate that would belong outside window if collocate were spread evenly)
\item         E21 =     "    "      (proportion of other words that would belong in window if collocate were spread evenly)
\item         E22 =     "    "      (proportion of other words that would belong outside window if collocate were spread evenly)
}

Conversion from N, n, K, k notation :

        -----------------------------------------
        |              | word | ¬ word    |  T  |
        -----------------------------------------
        | subcorpus    | k    | n-k       | n   |
        -----------------------------------------
        | ¬ subcorpus  | K-k  | N-K-(n-k) | N-n |
        -----------------------------------------
        | Totals       | K    | N-K       | N   |
        -----------------------------------------

Conversion to N, n, K, k notation :

\itemize{
  \item N = N
  \item n = O11 + O12
  \item K = O11 + O21
  \item k = O11
}

}

\value{
Each association measure return a numeric vector indicating, for each
corresponding index in the arguments, the association strengh between the word
under scrutiny and the subcorpus.

These association measures, unless otherwise stated in the help page of the
function, are positive when the word is over-represented ("attracted"), and
negative when the word is under-represented.

In absolute value, the more the word is over-representend or under-represented,
the more the association measure givien is hight.
}



\section{Author}{
  Bernard Desgraupes \cr
  \email{bernard.desgraupes@u-paris10.fr}\cr
  University of Paris Ouest - Nanterre - Lab Modal'X \cr \cr

  Sylvain Loiseau \cr
  \email{sylvain.loiseau@univ-paris13.fr} \cr
  University Paris-13 - Lab LDI \cr \cr
}

\references{TODO}

\seealso{TODO}

\keyword{linguistics, word association}
