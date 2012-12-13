# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load a matrix
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## The method writeMM and readMM in package Matrix do not write and read
## the rownames and colnames.

readSparseMatrix <- function(filename, rownames_filename, colnames_filename) {
   v <- scan(filename);
   i <- v[seq(from=1,to=length(v),by=3)]
   j <- v[seq(from=2,to=length(v),by=3)]
   x <- v[seq(from=3,to=length(v),by=3)]

   obsnames <- scan(rownames_filename, what="character", sep="\n");
   varnames <- scan(colnames_filename, what="character", sep="\n");
   mat <- spMatrix(length(obsnames), length(varnames), i=i, j=j, x=x);
   rownames(mat) <- obsnames;
   colnames(mat) <- varnames;
   return(mat);
}

# TODO : utiliser write plutôt
writeSparseMatrix <- function(m, filename) {
   s <- summary(m)
   values <- data.frame(s$i, s$j, s$x)
   write.table(values, filename, row.names = FALSE, col.names = FALSE)
   write(rownames(m), paste(filename, "rownames", sep=""), sep="\n");
   write(colnames(m), paste(filename, "colnames", sep=""), sep="\n");
}


