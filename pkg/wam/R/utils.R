
# turn a flat list representation into a contingency table representation

# 
make.contingency <- function(N, n, K, k) {
    c <- matrix(c(k, K-k, n-k, (N-K)-(n-k)), 2, 2, dimnames=list(c("n", "!n"), c("k", "!k")));
    return(c);
}

