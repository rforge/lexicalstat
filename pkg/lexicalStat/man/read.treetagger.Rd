\name{read.treetagger}
\alias{read.treetagger}
\title{Read a tabulated file as produced by treetagger}
\usage{
  read.treetagger(file, contains.xml = TRUE,
    discard.xml = FALSE)
}
\arguments{
  \item{file}{filename for featching the file}

  \item{contains.xml}{does the file contains XML element?}

  \item{discard.xml}{if \code{contains.xml=TRUE}, are the
  XML elements kept?}
}
\description{
  Read a file produced by treetagger and convert it into a
  dataframe of class \code{\link{Tabulated}}.
}
\details{
  If XML tags are found in the file, they are represented
  "ala CWB" (see documentation of class "tabulated"). In
  extra column of the data frame: each element has it own
  column and each start/end tag pair is representend as an
  id in this column. For instance, the following file :
  \samp{ <?xml version="1.0" encoding="UTF-8"?> <EntryFree>
  <form> baba ADJ baba </form> <def> Gâteau NOM gâteau dans
  PRP dans lequel PRO:REL lequel il PRO:PER il entre
  VER:pres entrer du PRP:det du cédrat NOM cédrat , PUN ,
  du PRP:det du raisin NOM raisin de PRP de Corinthe NAM
  Corinthe et KON et du PRP:det du rhum NOM rhum ou KON ou
  du PRP:det du kirsch NOM kirsch .  SENT . </def>
  </EntryFree> <EntryFree> <form> babeurre NOM babeurre
  </form> <def> Liquide NAM <unknown> séreux ADJ séreux qui
  PRO:REL qui reste VER:pres rester après PRP après le
  DET:ART le barattage NOM barattage de PRP de la DET:ART
  le crème NOM crème .  SENT . </def> } Results in the
  following data frame : \samp{ word pos lemma cit def
  EntryFree form 3 baba ADJ baba 0 0 1 1 6 Gâteau NOM
  gâteau 0 1 1 0 7 dans PRP dans 0 1 1 0 8 lequel PRO:REL
  lequel 0 1 1 0 9 il PRO:PER il 0 1 1 0 10 entre VER:pres
  entrer 0 1 1 0 11 du PRP:det du 0 1 1 0 12 cédrat NOM
  cédrat 0 1 1 0 13 , PUN , 0 1 1 0 14 du PRP:det du 0 1 1
  0 15 raisin NOM raisin 0 1 1 0 16 de PRP de 0 1 1 0 17
  Corinthe NAM Corinthe 0 1 1 0 18 et KON et 0 1 1 0 19 du
  PRP:det du 0 1 1 0 20 rhum NOM rhum 0 1 1 0 21 ou KON ou
  0 1 1 0 22 du PRP:det du 0 1 1 0 23 kirsch NOM kirsch 0 1
  1 0 24 .  SENT .  0 1 1 0 29 babeurre NOM babeurre 0 0 2
  2 32 Liquide NAM <unknown> 0 2 2 0 33 séreux ADJ séreux 0
  2 2 0 34 qui PRO:REL qui 0 2 2 0 35 reste VER:pres rester
  0 2 2 0 36 après PRP après 0 2 2 0 37 le DET:ART le 0 2 2
  0 38 barattage NOM barattage 0 2 2 0 39 de PRP de 0 2 2 0
  40 la DET:ART le 0 2 2 0 41 crème NOM crème 0 2 2 0 42 .
  SENT .  0 2 2 0 }
}
\examples{
fileName <- system.file(c("inst", "exempleData"), "sample.ttg", package="lexicalStat")
c <- read.treetagger(fileName);
}

