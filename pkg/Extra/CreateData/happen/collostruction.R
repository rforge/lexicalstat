happen <- matrix(c(14, 21, 8606, 10197659), nrow=2, byrow = TRUE, dimnames=list(c("N waiting to happen", "!N waiting to happen"), c("accident", "!accident")))
save(happen, file="happen.rda")