# ===========================================================================
# File: "main.R"
#                        Created: 2013-04-05 14:55:29
#              Last modification: 2013-04-05 14:55:29
# Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
#          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
# This file is part of the wam project.
# ===========================================================================

## 
 # ------------------------------------------------------------------------
 # 
 # "cphyper(x)" --
 # 
 # Example:
 #      > cphyper(q, m, n, k)
 # 
 # ------------------------------------------------------------------------
 ##
cphyper <- function(q, m, n, k, lower.tail = TRUE, log.p = FALSE) {
	ans <- .Call("wam_cphyper", q, m, n, k, lower.tail, log.p, PACKAGE="wam")
    return(ans)
}



