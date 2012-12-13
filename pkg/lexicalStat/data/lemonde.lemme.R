library(Matrix);
#library(dicograph);

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

lemonde.lemme <- as.matrix(readSparseMatrix("lemonde.lemme.table", "lemonde.lemme.rownames", "lemonde.lemme.colnames"));
