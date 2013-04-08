# ===========================================================================
# File: "main.R"
#                        Created: 2013-03-26 17:45:06
#              Last modification: 2013-03-29 20:11:37
# Author: Bernard Desgraupes
# e-mail: <bernard.desgraupes@u-paris10.fr>
# This file is part of the specif project.
# ===========================================================================


## 
 # ------------------------------------------------------------------------
 # 
 # "specif_density" --
 # 
 # Plot the density of the hypergeometric distribution wrt the number of
 # observed tokens in the given part in a reasonable range around the mode.
 # 
 # ------------------------------------------------------------------------
 ##
specif_density <- function(parts, data, token="peuple", R=10, new=TRUE, ...) 
{
	if (missing(parts)) {
		np <- dim(data)[2]
		parts <- 1:np
	}
	
	nc <- length(parts)
	dsz <- colSums(data)
	N <- sum(dsz)
	K <- sum(data[token,])
	colnm <- colnames(data)
	
	nr <- 1
	nr <- 1
	if (nc == 1) {
		wd <- 6
	} else {
		if (nc > 2) {
			nr <- 2
		}
		if ( (nc %% 2) == 1) {
			nc <- nc + 1
		}
		wd <- 5+2*(nc/nr - 1) 
	}

	if (new) {
		dev.new(width=wd, height=6)
	}
	
	layout(matrix(1:nc, nrow=nr, byrow=TRUE))
	
	for (d in parts) {
		n <- dsz[d]
		k <- data[token,d]
		
		# Calculate the mode
		mo <- floor((n+1)*(K+1)/(N+2))
		
		# Keep R values on both sides of the mode
		if (k <= mo) {
			m <- min(c(k-1, mo-R))
			M <- mo+R
		} else {
			m <- mo-R
			M <- max(c(k+1, mo+R))
		}
		if (m < 0) {m <- 0}
		if (M > K) {M <- K}

		x <- m:M
		plot(x, dhyper(x, K, N-K, n), 'h', main=paste(colnm[d], ": n = ", n, sep=""),
			sub=paste("mode =",mo), xlab=paste("k=",k), ylab="", ...)
		
		points(k, dhyper(k, K, N-K, n), col="red")
	}
	
	layout(1)
}



## 
 # ------------------------------------------------------------------------
 # 
 # "specif_cdf" --
 # 
 # Plot the cdf function of the hypergeometric distribution wrt the number of
 # observed tokens in the given part in a reasonable range around the mode.
 # 
 # ------------------------------------------------------------------------
 ##
specif_cdf <- function(parts, data, token="peuple", R=10, new=TRUE, method="base", ...) 
{
	if (missing(parts)) {
		np <- dim(data)[2]
		parts <- 1:np
	}
	
	nc <- length(parts)
	dsz <- colSums(data)
	N <- sum(dsz)
	K <- sum(data[token,])
	colnm <- colnames(data)

	nr <- 1
	if (nc == 1) {
		wd <- 6
	} else {
		if (nc > 2) {
			nr <- 2
		}
		if ( (nc %% 2) == 1) {
			nc <- nc + 1
		}
		wd <- 5+2*(nc/nr - 1) 
	}

	if (new) {
		dev.new(width=wd, height=6)
	}	
	layout(matrix(1:nc, nrow=nr, byrow=TRUE))
	
	for (d in parts) {
		n <- dsz[d]
		
		# Calculate the mode
		mo <- floor((n+1)*(K+1)/(N+2))
		
		# Observed frequency
		k <- data[token,d]
		
		# Keep R values on both sides of the mode
		if (k <= mo) {
			m <- min(c(k-1, mo-R))
			M <- mo+R
		} else {
			m <- mo-R
			M <- max(c(k+1, mo+R))
		}
		if (m < 0) {m <- 0}
		if (M > K) {M <- K}
		
		klf <- m:mo
		plot(klf, phyper(klf, K, N-K, n), 'o', xlim=c(m,M), ylim=c(0,1), 
			main=paste(token, " - ", colnm[d], ": n = ", n, sep=""),
			sub=paste("mode =",mo), xlab=paste("k=",k), 
			ylab="", col="blue", pch=20, ...)
		krt <- mo:M
		lines(krt, phyper(krt, K, N-K, n), 'o', col="lightblue", pch=20, ...)
		klf <- m:(mo+1)
		lines(klf, 1-phyper(klf-1, K, N-K, n), 'o', col="lightpink", pch=20, ...)
		krt <- (mo+1):M
		lines(krt, 1-phyper(krt-1, K, N-K, n), 'o', col="red", pch=20, ...)
		legend("right",legend=c("spec","mode"), pch=c(5,2), cex=0.75)

		# Specificity for observed value
		spec <- ifelse(k <= mo, phyper(k, K, N-K, n), 1-phyper(k-1, K, N-K, n))
		pmo <- phyper(mo, K, N-K, n)
		points(c(k, mo), c(spec, pmo), pch=c(5,2))
		abline(h=0, col="lightgray")
		points(c(k, mo), c(0,0), pch=c(5,2))
		
		S <- specif_value(N, n, K, k, method=method) 
		text(m,0.5, paste("S =",round(S,3)), pos=4)
	}

	layout(1)
}



## 
 # ------------------------------------------------------------------------
 # 
 # "specif_calc" --
 # 
 # Possible methods: see specif_value()
 # 
 # Calculate the specificities in various parts.
 # 
 # ------------------------------------------------------------------------
 ##
specif_calc <- function(parts, data, token="peuple", method="base") 
{
	if (missing(parts)) {
		nc <- dim(data)[2]
		parts <- 1:nc
	}
	
	np <- length(parts)	
	dsz <- colSums(data)
	N <- sum(dsz)
	K <- sum(data[token,])
	colnm <- colnames(data)
	
	S <- vector(length=np)
	names(S) <- colnm[parts]
	i <- 1
	
	for (d in parts) {
		n <- dsz[d]
		
		# Observed frequency
		k <- data[token,d]

		S[i] <- specif_value(N, n, K, k, method)
		i <- i+1
	}
	return(S)
}



## 
 # ------------------------------------------------------------------------
 # 
 # "specif_value <- function(N, n, K, k, method="base")" --
 # 
 # Compute a specificity using the specified method.
 #     N, n, K are integers.
 #     k is an integer vector of observed values.
 # 
 # Possible methods: "base", "log", "gap", "scale", "logscale".
 # 
 # ------------------------------------------------------------------------
 ##
specif_value <- function(N, n, K, k, method="base") 
{
	# Calculate the mode
	islog <- (method %in% c("log","logscale"))
	mo <- floor((n+1)*(K+1)/(N+2))
	cdmo <- phyper(mo, K, N-K, n, log.p=islog)
	
	res <- vector(mode="numeric", length=length(k))
	
	# Specificity for observed value
	cdk <- ifelse(k <= mo, phyper(k, K, N-K, n, log.p=islog), phyper(k-1, K, N-K, n, log.p=islog, lower.tail=FALSE))
	
	res <- switch(method,
		"base"=ifelse(k <= mo, -cdk, cdk),
		"log"=ifelse(k <= mo, -abs(cdmo-cdk), abs(cdmo-cdk)),
		"gap"=ifelse(k <= mo, -abs(cdmo-cdk), abs(cdmo-cdk)),
		"scale"=ifelse(k <= mo, -abs(cdmo-cdk)/cdmo, abs(cdmo-cdk)/cdmo),
		"logscale"=ifelse(k <= mo, -abs((cdmo-cdk)/cdk) , abs((cdmo-cdk)/cdk))
	)
	
	return(res)
}



## 
 # ------------------------------------------------------------------------
 # 
 # "specif_plot" --
 # 
 # Plot the value of the specificity wrt the number of observations in a
 # part using a given method. If the radius is not specified, take twice
 # the standard deviation of the hypergeometric distribution.
 # 
 # ------------------------------------------------------------------------
 ##
specif_plot <- function(d, data, token="peuple", R=NULL, method="base", ...) 
{
	if (length(d) > 1) {
		stop("first arg must be a single part index.")
	}
	
	dsz <- colSums(data)
	N <- sum(dsz)
	K <- sum(data[token,])
	colnm <- colnames(data)
	n <- dsz[d]

	if (is.null(R)) {
		p <- K/N
		var <- n*p*(1-p)*(N-n)/(N-1)
		R <- floor(2*sqrt(var))+1
	}

	# Calculate the mode
	mo <- floor((n+1)*(K+1)/(N+2))

	# Adjust the range
	m <- max(c(0, mo-R))
	M <- mo+R
	
	x <- m:M
	y <- specif_value(N, n, K, x, method=method)
	
	plot(x, abs(y), type='o', main=paste(token,"-", colnm[d],"- method:",method),
		xlab=paste("mode =",mo), ylab="abs(specif)", ...)
	
	abline(h=0,v=mo,col="lightgray")
}



## 
 # ------------------------------------------------------------------------
 # 
 # "specif_cdmo" --
 # 
 # Calculate the value F(mo) of the cdf function at the mode for a given
 # token in different parts. This is the reference value of non-specificity.
 # 
 # ------------------------------------------------------------------------
 ##
specif_cdmo <- function(parts, data, token="peuple") 
{
	if (missing(parts)) {
		nc <- dim(data)[2]
		parts <- 1:nc
	}
	
	np <- length(parts)	
	dsz <- colSums(data)
	N <- sum(dsz)
	K <- sum(data[token,])
	colnm <- colnames(data)
	
	S <- vector(length=np)
	names(S) <- colnm[parts]
	i <- 1
	
	for (d in parts) {
		n <- dsz[d]
		mo <- floor((n+1)*(K+1)/(N+2))
		S[i] <- phyper(mo, K, N-K, n)
		i <- i+1
	}
	return(S)
}



## 
 # ------------------------------------------------------------------------
 # 
 # "specif_mode2density" --
 # 
 # Plot the behaviour of the density function at the mode value using
 # different sample sizes, i-e different modes.
 # 
 # ------------------------------------------------------------------------
 ##
specif_mode2density <- function(data, maxi=sum(data), token="peuple", ...) 
{
	K <- sum(data[token,])
	N <- sum(data)
	J <- (N+2)/(K+1)

	if (missing(maxi)) {
		maxi <- N
	}
	if (maxi > N) {
		maxi <- N
	}
	
	# Mode increases by 1 at each jump J
	x <- round(seq(K, maxi, J))

	# Calculate the mode
	mo <- floor((x+1)*(K+1)/(N+2))
	
	# Corresponding mass density
	md <- dhyper(mo, K, N-K, x)
	
	plot(mo, md, 'l', main=expression(P(X<=mo)),
		xlab="Modes", ylab="")
}



## 
 # ------------------------------------------------------------------------
 # 
 # "specif_mode2cdf" --
 # 
 # Plot the behaviour of the cdf function at the mode value using different
 # sample sizes, i-e different modes.
 # 
 # ------------------------------------------------------------------------
 ##
specif_mode2cdf <- function(data, maxi=sum(data), token="peuple", ...) 
{
	K <- sum(data[token,])
	N <- sum(data)
	J <- (N+2)/(K+1)

	if (missing(maxi)) {
		maxi <- N
	}
	if (maxi > N) {
		maxi <- N
	}
	
	# Mode increases by 1 at each jump J
	x <- round(seq(K, maxi, J))

	# Calculate the mode
	mo <- floor((x+1)*(K+1)/(N+2))
	
	# Corresponding cdf
	cd <- phyper(mo, K, N-K, x)
	
	plot(mo, cd, 'l', main=expression(P(X<=mo)),
		xlab="Modes", ylab="")
}
