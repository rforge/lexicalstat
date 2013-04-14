# ===========================================================================
# File: "wam.specificities.R"
#                        Created: 2013-04-05 14:55:29
#              Last modification: 2013-04-08 11:59:45
# Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
#          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
# This file is part of the wam project.
# ===========================================================================

wam.specificities <- function(N, n, K, k, method="log") {
	# mode
	mo <- floor((n+1)*(K+1)/(N+2));
	
	# cdf (or its log)
	islog <- (method %in% c("log","logscale"))
	cdk <- ifelse(k <= mo, phyper(k, K, N-K, n, log.p=islog), phyper(k-1, K, N-K, n, log.p=islog, lower.tail=FALSE));
	specif <- double(length(k));
	
	if (method == "base") {
		specif <- ifelse(k <= mo, -cdk, cdk);
	} else { 
		# cumulative probability for the mode (or its log)
		cdmo <- phyper(mo, K, N-K, n, log.p=islog);
		if (method == "gap") {
			specif <- ifelse(k <= mo, -abs(cdmo-cdk), abs(cdmo-cdk));
		} else if (method == "log") {
			specif <- ifelse(k <= mo, -abs(cdmo-cdk), abs(cdmo-cdk));
		} else if (method == "scale") {
			specif <- ifelse(k <= mo, -abs(cdmo-cdk)/cdmo, abs(cdmo-cdk)/cdmo);
		} else if (method == "logscale") {
			specif <- ifelse(k <= mo, -abs((cdmo-cdk)/cdk) , abs((cdmo-cdk)/cdk));
		} 
	}

	return(specif);
}

