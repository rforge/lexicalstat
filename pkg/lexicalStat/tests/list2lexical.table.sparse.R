library(textcorpus);
l <- list(
paste("A", 1:3, sep=""),
paste("B", 1:3, sep=""),
c(paste("A", 1:3, sep=""), "A1")
);

list2lexical.table.sparse(l);

names(l) <- c("part1", "part2", "part3");
list2lexical.table.sparse(l);

l2 <- list(
    c("A", "B", "C"),
    c("A", "A"),
    c("A", "B", "D")
    );

expected <- spMatrix(3, 4, c(1, 1, 1, 2, 3, 3, 3), c(1, 2, 3, 1, 1, 2, 4), c(1, 1, 1, 2, 1, 1, 1));
rownames(expected) <- NULL;
colnames(expected) <- c("A", "B", "C", "D")

found <- list2lexical.table.sparse(l2);

identical(expected, found);
