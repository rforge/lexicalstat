
# turn a flat list representation into a contingency table representation

# 
make.contingency <- function(N, n, K, k) {
    c <- matrix(c(k, K-k, n-k, (N-K)-(n-k)), 2, 2, dimnames=list(c("n", "N-n"), c("K", "N-K")));
    return(c);
}

make.list <- function(contingency) {
	if (nrow(contingency) != 2) stop("contingency must have 2 rows");
	if (ncol(contingency) != 2) stop("contingency must have 2 rows");
	if (!is.numeric(contingency)) stop("contingency must be numeric");

	N <- sum(contingency);
	n <- rowSums(contingency)[1];
	k <- contingency[1,1];
	K <- colSums(contingency)[1];
	return(data.frame(N, n, K, k));
}