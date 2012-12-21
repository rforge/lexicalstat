\name{lemonde.gram}
\alias{lemonde.gram}
\docType{data}
\title{
  Frequencies of morphosyntactic tags in 840 newspaper articles
}
\description{
  The tagging is produced by Syntex parser for French (see References below).
}

\usage{data(lemonde.gram)}
\format{
  A matrix with 840 observations and 93 variables.
}
\details{
    The tagset contains the following tags: "Adj??" "AdjMS" "Adj?P" "Adv" "AdvGP" "CCoord" "CCoordAdj" "CCoordPrepDe" "CSub" "Det" "Det??" "DetFS" "DetMP" "DetMS" "Elim" "Nom??" "NomFS" "NomInc" "NomM?" "NomMS" "Nom?P" "NomPr" "NomPrXXInc" "NomPrXXPrenom" "Nom?S" "NomXXDate" "Ppa??" "PpaFP" "PpaFS" "PpaMP" "PpaMS" "PpaMSp" "Ppr" "Prep" "PrepDet" "Pro" "ProRel" "Typo" "VCONJP" "VCONJS" "VINF" "AdjFS" "CCoordNom" "CCoordNomPr" "CCoordVCONJP" "CCoordVCONJS" "DetNum" "NomMP" "NomXXMes" "NomXXMon" "TypoCoordNom" "TypoCoordPpa" "TypoCoordPrepDe" "VCONJPp" "CCoordAdv" "CCoordPrep" "CCoordVCONJSp" "NomXXNum" "TypoCoordPrep" "TypoCoordVCONJSp" "VCONJSp" "AdjFP" "AdjM?" "CCoordCSub" "CCoordNomXXDate" "VINFp" "AdjMP" "CCoordDet" "CCoordProRel" "CCoordVINF" "TypoCoordNomPr" "TypoCoordVCONJS" "NomFP" "Adj?S" "CCoordNomXXNum" "TypoCoordAdj" "CCoordPpa" "TypoCoordVINF" "CCoordPrepDet" "TypoCoordPrepDet" "Pprp" "TypoCoordAdv" "NomXXAdr" "TypoCoordVCONJP" "CCoordVCONJPp" "TypoCoordProRel" "TypoCoordNomXXDate" "CCoordPpr" "NomXXHeure" "NomXXUrl" "CCoordVINFp" "TypoCoordNomXXNum" "NomXXMail"
}

\source{
    Loiseau S. (2010) « Investigating the interactions between different axes of variation in text typology », \emph{in} Grzybek P. & Kelih E. (ed.) \emph{Text and Language: Structures, Functions, Interrelations}.
}

\references{
Bourigault D., Fabre C., Fr\'erot C., Jacques M.-P. and Ozdowska S. (2005), Syntex, analyseur syntaxique de corpus, in \emph{Actes des 12\'emes journ\'ees sur le Traitement Automatique des Langues Naturelles}, Dourdan, France.
}

\examples{
data(lemonde.gram)
data(lemonde.info)

by.genre <- rowsum(lemonde.gram, lemonde.info$genre)
p <- princomp(t(by.genre))
# biplot(p);

}
\keyword{datasets}
