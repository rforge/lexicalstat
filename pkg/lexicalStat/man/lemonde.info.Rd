\name{lemonde.info}
\alias{lemonde.info}
\docType{data}
\title{
  Information about 840 newspaper articles.
}
\description{
  This is a companion for the data set  \code{\link{lemonde.lemme}} and
  \code{\link{lemonde.gram}}; it provides, for each articles, extra linguistic
  pieces of information.
}

\usage{data(lemonde.info)}

\format{
  A data frame with 840 observations on the following 4 variables.
  \describe{
    \item{\code{year}}{a factor with level from "1987" to "2002".}
    \item{\code{section}}{a factor with levels \code{ART} \code{ECO-ENT} \code{HOR} \code{INT-ETR} \code{POL-FRA} \code{SPO}}
    \item{\code{section.orig}}{The original names of the sections, before regrouping sections with different names accross the time. \code{ECO-ENT} \code{INT-ETR} \code{POL-FRA} are divided into two sections}
    \item{\code{genre}}{a factor with levels \code{BIOGRAPHIE} \code{CORRESPONDANCE} \code{ENTRETIEN} \code{NECROLOGIE} \code{OPINION} \code{PORTRAIT}}
    \item{\code{author}}{a factor with levels \code{CATINCHI PHILIPPE JEAN} \code{CHEMIN ARIANE} \code{CONDE GERARD} \code{COURTOIS GERARD} \code{DECAMPS MARIE CLAUDE} \code{DHOMBRES DOMINIQUE} \code{DUMAY JEAN MICHEL} \code{GEORGES PIERRE} \code{GROSRICHARD FRANCOIS} \code{ISNARD JACQUES} \code{KAUFFMANN SYLVIE} \code{KECHICHIAN PATRICK} \code{LABE YVES MARIE} \code{NAU JEAN YVES} \code{ROLLAT ALAIN} \code{ROSENZWEIG LUC} \code{TINCQ HENRI}}
    \item{\code{years.group}}{grouping years into three groups. The first one is from 1987 to 1991, the second from 1992 to 1996, the last one from 1997 to 2002.}
    }
}

\details{
    ART: art; ECO-ENT: economy and enterprise; HOR: horizon (opinion); INT-ETR: "international-\'etranger" (international); POL-FRA politique, France; SPO: sport
}

\source{
    Loiseau S. (2010) « Investigating the interactions between different axes of variation in text typology », \emph{in} Grzybek P. & Kelih E. (ed.) \emph{Text and Language: Structures, Functions, Interrelations}.
}

\examples{

data(lemonde.info)
data(lemonde.gram)

# get information about an article
lemonde.info[5,]

# group the articles by genres
by.genre <- rowsum(lemonde.gram, lemonde.info$genre);

}
\keyword{datasets}
