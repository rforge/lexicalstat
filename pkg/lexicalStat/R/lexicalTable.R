# TODO : replace sparse.matrix.R
# replace corpus.R (contenu repris)

setClass("LexicalTable",
         contains = "dgCMatrix",
         validity = function(object) {
           if (class(object) == "LexicalTable")
             return(TRUE)
           else
             return(FALSE)
         })

lexicalTable <- function(table) {
  if (is.data.frame(table)) {
    table <- as.matrix(table);
    if (!is.numeric(table)) {
      stop("cannot create lexicalTable with a non-numeric data.frame");
    }
  }
  m <- 0;
  if (class(table) == "dgCMatrix") {
    m <- table;
  } else if (is.matrix(table)) {
    m <- Matrix(table, sparse = TRUE);
  } else if (class(table) == "frequencyList") {
     stop("Not implemented yet");
  } else if (class(table) == "fullText") {
      stop("Not implemented yet");
  } else {
    stop(paste("don't know how to make a lexicalTable with a", class(matrix)));
  }
  return(new("LexicalTable", m));
}

printLexicalTable <- function(x) {
  cat(paste("A lexical table\n"));
}

setMethod("print", signature(x="LexicalTable"), printLexicalTable)

summaryLexicalTable <- function(object){
  cat(paste("A lexical table:\n"));
  cat(paste("Number of parts (columns):", ncol(object), "\n"));
  cat(paste("Number of forms (rows):", nrow(object), "\n"));
  cat(paste("Number of tokens:", sum(object), "\n"));
  invisible(x);
}

setMethod("summary", signature(object = "LexicalTable"), summaryLexicalTable)

readLexicalTable <- function(basename) {
   freq.filename <- paste(basename, "forms", sep="freq");
   forms.filename <- paste(basename, "forms", sep=".");
   parts.filename <- paste(basename, "parts", sep=".");

   v <- scan(freq.filename);
   i <- v[seq(from=1,to=length(v),by=3)]
   j <- v[seq(from=2,to=length(v),by=3)]
   x <- v[seq(from=3,to=length(v),by=3)]

   obsnames <- scan(forms.filename, what="character", sep="\n");
   varnames <- scan(parts.filename, what="character", sep="\n");
   mat <- spMatrix(length(obsnames), length(varnames), i=i, j=j, x=x);
   rownames(mat) <- obsnames;
   colnames(mat) <- varnames;

   lexicalTable <- lexicalTable(mat);

   return(lexicalTable);
}

writeLexicalTable <- function(obj, file) {
   freq.filename <- paste(file, "forms", sep="freq");
   forms.filename <- paste(file, "forms", sep=".");
   parts.filename <- paste(file, "parts", sep=".");

   s <- summary(x)
   values <- data.frame(s$i, s$j, s$x)
   write.table(values, freq.filename, row.names = FALSE, col.names = FALSE)

   write(rownames(x), forms.filename, sep="\n");
   write(colnames(x), parts.filename, sep="\n");
}

setGeneric("write", function(obj, file) {
  return(standardGeneric("write"));
})

setMethod("write", "LexicalTable", writeLexicalTable);

asLexicalTable <- function(x, ...) UseMethod("asLexicalTable");

asLexicalTable.tabulated <- function(tabulated, positional, structural) {
  if (! "part" %in% colnamaes(tabulated)) {
    stop("part not known");
  }
  positional.factor <- tabulated[, positional];
  structural.factor <- tabulated[, structural];
  forms <- levels(positional.factor);
  parts <- levels(structural.factor);

  f <- count(data.frame(positional.factor, structural.factor));
  i <- as.numeric(f[,1]);
  j <- as.numeric(f[,2]);

  m <- sparseMatrix(i=i, j=j, x=f[,3]);
  rownames(m) <- forms;
  colnames(m) <- parts;

  return(lexicalTable(m));
}

asLexicalTable.fullText <- function(fullText) {
  debug <- FALSE;
  if (debug) print("[list2lexical.table.sparse] checking argument...");
  all.character <- sapply(fullText, is.character);
  if (!all(all.character)) stop("all element of the list must be character vector of token");
  if (debug) print("[list2lexical.table.sparse] ...forms and tokens...");

  tokens <- unlist(fullText);
  forms <- unique(tokens);

  if (debug) print("[list2lexical.table.sparse] ...tables and tables length...");
  #part.lengths <- sapply(fullText, length);
  part.tables <- lapply(fullText, table);
  part.table.lengths <- sapply(part.tables, length);

  if (debug) print("[list2lexical.table.sparse] ...i...");
  i <- unlist(sapply(part.tables, function(t) which(forms %in% names(t))));
  if (debug) print("[list2lexical.table.sparse] ...j...");
  j <- rep(1:length(fullText), part.table.lengths);
  if (debug) print("[list2lexical.table.sparse] ...v...");
  v <- unlist(sapply(part.tables, as.numeric));

  if (debug) print("[list2lexical.table.sparse] ...creating matrix...");
  #m <- spMatrix(nrow=length(fullText), ncol=length(forms), i=i, j=j, x=v);
  m <- sparseMatrix(i=i, j=j, x=v);
  rownames(m) <- forms;
  colnames(m) <- names(fullText);
  return(lexicalTable(m));
}
