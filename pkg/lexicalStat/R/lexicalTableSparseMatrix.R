setClass("LexicalTableSparseMatrix",
         contains = c("dgCMatrix", "LexicalTable"), # dgCMatrix sparseMatrix
         validity = function(object) {
           if (class(object) == "LexicalTableSparseMatrix")
             return(TRUE)
           else
             return(FALSE)
         })

setIs("LexicalTableSparseMatrix", "LexicalTable");

#> x <- spMatrix(ncol=10, nrow=10, i=1:10, 1:10, x=1:10)
#> x
#10 x 10 sparse Matrix of class "dgTMatrix"
#
#> y <- Matrix(robespierre, sparse=TRUE)
#> y
#6 x 10 sparse Matrix of class "dgCMatrix"
#
#> z <- sparseMatrix(i=10, j=10, x=10)
#> z
#10 x 10 sparse Matrix of class "dgCMatrix"

############################################################
##
## Implementation of Corpus
##
############################################################

##############################################################
#' @rdname N-methods
#' @aliases N,LexicalTableSparseMatrix-method
setMethod("N", "LexicalTableSparseMatrix", function(corpus) sum(corpus));

############################################################
##
## Implementation of CorpusAsFrequencies
##
############################################################

##############################################################
#' @rdname ntype-methods
#' @aliases ntype,LexicalTableSparseMatrix-method
setMethod("ntype", "LexicalTableSparseMatrix", function(corpus) nrow(corpus));

##############################################################
#' @rdname types-methods
#' @aliases types,LexicalTableSparseMatrix-method
setMethod("types", "LexicalTableSparseMatrix", function(corpus) sort(rownames(corpus)));

############################################################
##
## Implementation of LexicalTable
##
############################################################

setMethod("subfreq", "LexicalTableSparseMatrix", function(obj, types, parts) {
    ntype <- length(types);
    npart <- length(parts);
    
    submat <- obj[types, parts];
    subfreq <- data.frame(
      Type=rep(types, npart),
      Part=rep(parts, times=rep(ntype, npart)),
      Subfrequency=as.vector(submat)
    );
    return(subfreq);
});

############################################################
##
## Utility functions
##
############################################################

setMethod("show", signature(object="LexicalTableSparseMatrix"), function(object) {
  printSpMatrix(object, col.names=T)
});

setMethod("print", signature(x="LexicalTableSparseMatrix"), function(x) {
  printSpMatrix(x, col.names=T)
})

setMethod("summary", signature(object = "LexicalTableSparseMatrix"), function(object) {
  cat(paste("A lexical table:\n"));
  cat(paste("Number of parts (columns):", ncol(object), "\n"));
  cat(paste("Number of types (rows):", nrow(object), "\n"));
  cat(paste("Number of tokens:", sum(object), "\n"));
  invisible(x);
})

############################################################
##
## read/write
##
############################################################

readLexicalTable <- function(basename) {
   freqs.filename <- paste(basename, "freqs", sep=".");
   types.filename <- paste(basename, "types", sep=".");
   parts.filename <- paste(basename, "parts", sep=".");

   if (any(!file.exists(c(freqs.filename, types.filename, parts.filename)))) {
     stop("cannot read or access some files");
   }

   v <- scan(freqs.filename);
   i <- v[seq(from=1,to=length(v),by=3)]
   j <- v[seq(from=2,to=length(v),by=3)]
   x <- v[seq(from=3,to=length(v),by=3)]

   obsnames <- scan(types.filename, what="character", sep="\n");
   varnames <- scan(parts.filename, what="character", sep="\n");
   mat <- sparseMatrix(i=i, j=j, x=x);
   rownames(mat) <- obsnames;
   colnames(mat) <- varnames;

   lexicalTable <- lexicalTable(mat);
   return(lexicalTable);
}

writeLexicalTable <- function(obj, file) {
   freqs.filename <- paste(file, "freqs", sep=".");
   types.filename <- paste(file, "types", sep=".");
   parts.filename <- paste(file, "parts", sep=".");

   s <- summary(x)
   values <- data.frame(s$i, s$j, s$x)
   write.table(values, freqs.filename, row.names = FALSE, col.names = FALSE)

   write(rownames(x), types.filename, sep="\n");
   write(colnames(x), parts.filename, sep="\n");
}

