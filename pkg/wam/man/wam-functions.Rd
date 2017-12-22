\name{wam-functions}
\alias{wam.ar}
%\alias{wam.fisher}
\alias{wam.chisq}
\alias{wam.frequency}
\alias{wam.jaccard}
\alias{wam.loglikelihood}
\alias{wam.MI}
\alias{wam.specificities}
\alias{wam.t}
\alias{wam.z}
\alias{wam.collostruction}
\title{
Calculate word association measure according to various fomula
}
\description{
Word association measures give an information about the tendency of two
words to co-occurr with greater (or lesser) than chance frequency.
}

\usage{
wam.ar(N, n, K, k)
%wam.fisher(N, n, K, k)
wam.collostruction(N, n, K, k)
wam.chisq(N, n, K, k, yates.correction, p.value, two.sided)
wam.frequency(N, n, K, k)
wam.jaccard(N, n, K, k)
wam.loglikelihood(N, n, K, k, p.value = FALSE, two.sided = TRUE)
wam.MI(N, n, K, k)
wam.specificities(N, n, K, k, method = "log")
wam.t(N, n, K, k)
wam.z(N, n, K, k, yates.correction = FALSE)
}
\arguments{
  \item{N}{
The total number of occurrences in the corpus
}
  \item{n}{
The frequency of word 1
}
  \item{K}{
The frequency of word 2
}
  \item{k}{
The frequency of the cooccurrence or word 1 and word 2.
}

% for chisq and Z
\item{yates.correction}{

}
% for chisq and loglikelihood
  \item{p.value}{

}
  \item{two.sided}{

}

% for spécificities
  \item{method}{
  the method used:
\itemize{
  \item base
  \item log
  \item logscale
  \item gap
  \item scale
}
}


}
\details{
\describe{
\item{wam.collostruction}{Calculate word association according to the collostruction method defined in Stefanowitsch and Gries 2003.}

\item{wam.ar}{Calculate word association according to the association ration defined in Church and Hanks 1990.}
%\item{wam.fisher}{}
\item{wam.chisq}{Calculate word association according to the chi square test.}
\item{wam.frequency}{}
\item{wam.jaccard}{}
\item{wam.loglikelihood}{}
\item{wam.MI}{Calculate word association according to the information theoretic concept of Mutual information: \eqn{MI(x,y) =  P(x,y) / P(x) P(y)}}
\item{wam.specificities}{Calculate word association according to the specificities method (hypergeometric distribution).}
\item{wam.t}{Calculate word association according to the T test.
\verb{
t-score =  prob(X=chemistry, Y=physics) - ( prob(X=chemistry) prob(Y=physics) )  
           ----------------------------------------------------------------------
                         sqrt((1/T) prob(X=chemistry, Y=physics))
}
}
\item{wam.z}{}
}
}

\value{
A numeric values. All functions are vectorized.
}

\references{
\itemize{

% TODO \item sur collostruction : http://www.anglistik.uni-muenchen.de/personen/professoren/schmid/schmid_publ/collostructional-analysis.pdf

\item loglikelihood : DUNNING, Ted, 1993.  « Accurate methods for the
statistics of surprise and coincidence ».  In: \emph{Computational
linguistics}.  19/1.  MIT Press, pp.  61-74

% for z-score (t-score?) : for chi square : 
\item CHURCH, K., GALE, W., HANKS, P. & HINDLE, D., 1991.  «
Using Statistics in Lexical Analysis ».  In : ZERNIK, U. (ed.),
\emph{Lexical Acquisition}.  Hillsdale, Lawrence Erlbaum Ass., pp.
115--164

% For association ratio : 
\item Kenneth Ward Church, Patrick Hanks (1990) "Word Association Norms,
Mutual Information, and Lexicography" \emph{Computational Linguistics},
16/1, pages 22-29.
\url{http://www.aclweb.org/anthology/P89-1010.pdf}

% Specificities : 
\item Lafon, P. (1980). « Sur la variabilité de la fréquence des formes
dans un corpus ». In: \emph{Mots}. 1. pp. 127--165. \url{http://www.persee.fr/web/revues/home/prescript/article/mots_0243-6450_1980_num_1_1_1008}

% Collostruction
Stefanowitsch A. \& Gries St. Th. 2003 "Collostructions: Investigating the interaction of words and constructions", \emph{International Journal of Corpus Linguistics}, 8/2, 209-234.

}
}
\author{
Bernard Desgraupes
Sylvain Loiseau
}
\seealso{
see \code{\link{wam}} for another, higher level interface.
}
\examples{
data(ar)
attach(ar)
wam.ar(N, n, K, k)
}
