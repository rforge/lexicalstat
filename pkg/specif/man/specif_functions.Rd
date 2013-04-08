\name{specif_functions}
\alias{specif_mode2cdf}
\alias{specif_mode2density}
\alias{specif_density}
\alias{specif_cdf}
\alias{specif_plot}
\alias{specif_calc}
\alias{specif_value}
\alias{specif_cdmo}

\title{* Test specificities *}

\description{
	Utility functions to play with specificities.
}

\usage{
	specif_calc(parts, data, token="peuple", method="base")
	specif_value(N, n, K, k, method="base")
	specif_cdf(parts, data, token="peuple", R=10, new=TRUE, method="base", ...)
	specif_cdmo(parts, data, token="peuple")
	specif_density(parts, data, token="peuple", R=10, new=TRUE, ...)
	specif_mode2cdf(data, maxi=sum(data), token="peuple", ...)
	specif_mode2density(data, maxi=sum(data), token="peuple", ...)
	specif_plot(d, data, token="peuple", R=NULL, method="base", ...) 
}


\arguments{
	\item{parts}{vector of part indices}
	\item{d}{index of single part}
	\item{data}{table of frequencies}
	\item{token}{token for which to determine the specificity}
	\item{maxi}{maximum part size}
	\item{R}{radius of interval around mode}
	\item{new}{if TRUE start a new window device}
	\item{method}{method used to calculate the specificity}
	\item{...}{other arguments passed to plot functions}
	\item{N}{number of balls in the urn}
	\item{K}{number of white balls in the urn}
	\item{n}{number of balls drawn from the urn}
	\item{k}{vector of observed white balls}
}

\details{
	Utility functions to play with specificities.
	
	The possible method names are : "base", "log", "gap", "scale" or "logscale".

}

\value{
\code{specif_calc} returns a vector of specificities.
}

\author{
  Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr}\cr
  University of Paris Ouest - Nanterre - Lab Modal'X \cr \cr

  Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} \cr
  University Paris-13 - Lab LDI \cr \cr
}

\references{
	P. Lafon \emph{Sur la variabilité de la fréquence des formes dans un corpus}, Mots, octobre 1980, N°1. pp. 127-165.
}


\examples{
data(rob)
specif_calc(3:9, rob)
specif_density(1:10,rob)
dev.new()
specif_cdf(c(4,8),rob)
specif_plot(8,rob)
}

\keyword{specificity, hypergeometric}