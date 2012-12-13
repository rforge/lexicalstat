library(textcorpus);
l <- list(
paste("A", 1:3, sep=""),
paste("B", 1:3, sep=""),
c(paste("A", 1:3, sep=""), "A1")
);

list2lexical.table(l);

names(l) <- c("part1", "part2", "part3");
list2lexical.table(l);
