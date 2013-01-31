setClass("LexicalTableSparseMatrix",
         contains = "dgCMatrix", # dgCMatrix sparseMatrix
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
## Constructor
##
############################################################

lexicalTable <- function(mat) {
  if (is.data.frame(mat)) {
    mat <- as.matrix(mat);
    if (!is.numeric(mat)) {
      stop("cannot create lexicalTable with a non-numeric data.frame");
    }
  }
  m <- 0;
  if (is(mat, "sparseMatrix")) {
    m <- as(mat, "dgCMatrix");
  } else if (is.matrix(mat)) {
    m <- Matrix(mat, sparse = TRUE);
  } else if (class(mat) == "frequencyList") {
     stop("Not implemented yet");
  } else if (class(mat) == "fullText") {
      stop("Not implemented yet");
  } else if (class(mat) == "tabulated") {
      stop("Not implemented yet");
  } else {
    stop(paste("don't know how to make a lexicalTable with a", class(mat)));
  }

  if (is.null(rownames(m))) {
    stop("must have rownames");
  }
  if (is.null(colnames(m))) {
    stop("must have colnames");
  }

  return(new("LexicalTableSparseMatrix", m));
}

############################################################
##
## Implementation of Corpus
##
############################################################

setMethod("N", "LexicalTableSparseMatrix", function(obj) sum(obj));

setMethod("ntype", "LexicalTableSparseMatrix", function(obj) nrow(obj));

setMethod("types", "LexicalTableSparseMatrix", function(obj) sort(rownames(obj)));

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
