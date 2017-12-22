r <- read.csv("robespierre.csv")
r <- as.matrix(r)
n <- colSums(r)
K <- rowSums(r)

robespierre <- as.data.frame( as.table( r ) )
colnames(robespierre) <- c("types", "parts", "k")
robespierre$N <- sum(r);
robespierre$K <- K[robespierre$types]
robespierre$n <- n[robespierre$parts]
save(robespierre, file="robespierre.rda")