\name{prema.info}
\alias{prema.info}
\docType{data}
\title{
 Information about the corpus Premature
}
\description{
   Information about texts of corpus Premature (see \code{\link{prema}}).
}
\usage{data(prema.info)}
\format{
  A data frame with 1015 observations on the following 12 variables.
  \describe{
    \item{\code{visites}}{Did the mother visit the baby? A factor with levels \code{TSVT} (Very often) \code{SVT} (often) \code{RARE} (rarely) \code{NR} (not known) \code{NSP} (not known) \code{J1N} (?) \code{J1O} (?)}
    \item{\code{proninf}}{What is the prognosis of the nurse about the baby? A factor with levels \code{TB} (very good) \code{B} (good) \code{MAUV} (bad) \code{NR} (not known) \code{NSP} (not known)}
    \item{\code{position}}{Position of the baby. A factor with levels \code{CCOT} \code{CDOS} \code{CVEN} \code{PCOT} \code{PDOS} \code{PVEN} \code{NR}}
    \item{\code{moralinf}}{Personnal mood of the nurse at the time of visiting the baby. A factor with levels \code{TB} (very good) \code{B} (good) \code{MAUV} (bad) \code{NR} (not known)}
    \item{\code{clposition}}{? A factor with levels \code{VENT} \code{COTE} \code{DOS} \code{NR}}
    \item{\code{freqoccupation}}{? A factor with levels \code{JAMA} \code{NPER} \code{NR} \code{OCCA} \code{REGU}}
    \item{\code{ventilation}}{How the baby is ventilate? A factor with levels \code{INTU} \code{NON} \code{NR} \code{SNAS}}
    \item{\code{jour}}{number of days after birth at the time the nurse is visiting the baby. Numeric vector, one of 1, 3, 7, 15}
    \item{\code{sedation}}{Does the baby receive sedative? A factor with levels \code{CANA} \code{HYFE} \code{NON} \code{NR}}
    \item{\code{clpoids}}{? A factor with levels \code{i750} \code{ie1k} \code{s1kg}}
    \item{\code{merebb}}{? A factor with levels \code{B} \code{MAUV} \code{NR} \code{NSP} \code{TB}}
    \item{\code{infparents}}{? A factor with levels \code{B} \code{MAUV} \code{MOY} \code{NCCP}}
  }
}
\source{
   See. Habert B. (2009) \emph{Construire des bases de donn\'ees pour le français}, Ophrys, Paris.
}

\examples{
data(prema.info)
table(prema.info$proninf, prema.info$moralinf);
}

\keyword{datasets}
