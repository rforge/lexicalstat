\name{lemonde.lemme}
\alias{lemonde.lemme}
\docType{data}
\title{
   Lexical table of 840 newspaper articles.
}

\description{
  This contingency table give for 840 articles the frequency of 11274
  lemma. Only word with a total frequency greater than 3 have been kept (1/3 of
  the total number of words). The articles are selected from the French daily
  newspaper \emph{Le Monde} and illustrate various authors, genres and
  sections, from the year 1987 to the year 2002. The data set
  \code{\link{lemonde.info}} provide this information for each article. See
  also \code{\link{lemonde.gram}} for a contingency table of grammatical
  features.
}

\usage{data(lemonde.lemme)}

\format{
  829 rows (article) * 11,274 columns (lemmas).
  %This is a sparse matrix from the \code{\link{Matrix}} package.
}

\source{
    Loiseau S. (2010) « Investigating the interactions between different axes of variation in text typology », \emph{in} Grzybek P. & Kelih E. (ed.) \emph{Text and Language: Structures, Functions, Interrelations}.
}

%lexicon <- colSums(lemonde.lemme)
%names(lexicon) <- colnames(lemonde.lemme)
%
%sublexicon <- colSums(lemonde.lemme[lemonde.info$section == "SPORT", ]);
%names(sublexicon) <- colnames(lemonde.lemme)
%
%specificites.lexicon(lexicon, sublexicon)

\examples{
data(lemonde.lemme)
data(lemonde.info)

# size of the articles
hist(rowSums(lemonde.lemme), breaks=50)

# specificites of each genres
by.genre <- rowsum(lemonde.lemme, lemonde.info$genre);
s <- specificites(by.genre);
print(s, threshold=10);

# specificites of each of three groups of years
by.years.group <- rowsum(lemonde.lemme, lemonde.info$years.group);
s <- specificites(by.years.group);
print(s, threshold=10);
print(s, threshold=5, parts=1);

}
\keyword{datasets}
