############################################################
##
## As Tabulated
##
############################################################

setAs("FullText", "Tabulated", function(from, to) {
  x <- unlist(from);
  m <- data.frame(word=as.factor(x));
  y <- sapply(from, length);
  m[, "part"] <- as.factor(rep(0:(length(from) - 1), times=y));
  t <- tabulated(m, "word", "part");
  return(t);
});

# helpers

setGeneric("asTabulated", function(x) {
  return(standardGeneric("asTabulated"));
})

setMethod("asTabulated", "FullText", function(x) {
  as(x, "Tabulated");
});

############################################################
##
## As FullText
##
############################################################

# setAs("Tabulated", "FullText", function(from, to, positional, structural) {
#   if (is.null(positional)) {
#     stop("positional cannot be NULL");
#   }
#   if (is.null(structural)) {
#     stop("structural cannot be NULL");
#   }
# # TODO check if structural and positional exist
#   l <- split(from[,positional], from[,structural])
#   l <- lapply(l, as.character);
#   obj <- fullText(l);
#   return(obj);
# });

# helpers

setGeneric("asFullText", function(x, positional, structural) {
  return(standardGeneric("asFullText"));
})

setMethod("asFullText", c("Tabulated", "character", "character"), function(x, positional, structural) {
#      as(x, "FullText", positional, structural);
  from <- x;
  if (is.null(positional)) {
    stop("positional cannot be NULL");
  }
  if (is.null(structural)) {
    stop("structural cannot be NULL");
  }
# TODO check if structural and positional exist
  l <- split(from[,positional], from[,structural])
  l <- lapply(l, as.character);
  obj <- fullText(l);
  return(obj);
});

############################################################
##
## As LexicalTable
##
############################################################

setAs("FullText", "LexicalTable", function(from, to) {
  debug <- FALSE;
  if (debug) print("[list2lexical.table.sparse] checking argument...");
  all.character <- sapply(from, is.character);
  if (!all(all.character)) stop("all element of the list must be character vector of token");
  if (debug) print("[list2lexical.table.sparse] ...forms and tokens...");

  tokens <- unlist(from);
  forms <- unique(tokens);

  if (debug) print("[list2lexical.table.sparse] ...tables and tables length...");
  #part.lengths <- sapply(from, length);
  part.tables <- lapply(from, table);
  part.table.lengths <- sapply(part.tables, length);

  if (debug) print("[list2lexical.table.sparse] ...i...");
  i <- unlist(sapply(part.tables, function(t) which(forms %in% names(t))));
  if (debug) print("[list2lexical.table.sparse] ...j...");
  j <- rep(1:length(from), part.table.lengths);
  if (debug) print("[list2lexical.table.sparse] ...v...");
  v <- unlist(sapply(part.tables, as.numeric));

  if (debug) print("[list2lexical.table.sparse] ...creating matrix...");
  #m <- spMatrix(nrow=length(from), ncol=length(forms), i=i, j=j, x=v);
  m <- sparseMatrix(i=i, j=j, x=v);
  rownames(m) <- forms;
  n <- ifelse(is.null(names(from)), paste("part", 1:length(from), sep="_"), names(from))
  colnames(m) <- n;
  return(lexicalTable(m));
});

# setAs("LexicalTable", "Tabulate", function(from, to, positional, structural) {
#   if (! structural %in% attr(from, "structural")) {
#     stop("structural attribute not known");
#   }
#   positional.factor <- as.factor(from[, positional]);
#   structural.factor <- as.factor(from[, structural]);
#   forms <- levels(positional.factor);
#   parts <- levels(structural.factor);
# 
#   f <- count(data.frame(as.numeric(positional.factor), as.numeric(structural.factor)));
#   i <- f[,1];
#   j <- f[,2];
# 
#   m <- sparseMatrix(i=i, j=j, x=f[,3]);
#   rownames(m) <- forms;
#   colnames(m) <- parts;
# 
#   return(lexicalTable(m));
# });

# helpers

setGeneric("asLexicalTable", function(x, positional, structural) {
  return(standardGeneric("asLexicalTable"));
})

setMethod("asLexicalTable", c("Tabulated"), function(x, positional, structural) {
#      as(x, "LexicalTable", positional, structural);
  from <- x;
  if (! structural %in% attr(from, "structural")) {
    stop("structural attribute not known");
  }
  positional.factor <- as.factor(from[, positional]);
  structural.factor <- as.factor(from[, structural]);
  forms <- levels(positional.factor);
  parts <- levels(structural.factor);

  f <- count(data.frame(as.numeric(positional.factor), as.numeric(structural.factor)));
  i <- f[,1];
  j <- f[,2];

  m <- sparseMatrix(i=i, j=j, x=f[,3]);
  rownames(m) <- forms;
  colnames(m) <- parts;

  return(lexicalTable(m));
});

setMethod("asLexicalTable", c("FullText"), function(x) {
  as(x, "LexicalTable")
});

############################################################
##
## As FrequencyList
##
############################################################

#setAs("Tabulated", "FrequencyList", function(from, to, positional) {
#  if (! positional %in% positional(from)) {
#    stop("Unknown positional attribute");
#  }
#  freq <- table(from[,positional]);
#  return(frequencyList(freq));
#});

setAs("FullText", "FrequencyList", function(from, to) {
  freq <- table(unlist(from));
  return(frequencyList(freq));
});

setAs("LexicalTable", "FrequencyList", function(from, to) {
   fl <- rowSums(from);
   n <- rownames(from);
   names(fl) <- n;
   return(frequencyList(fl));
});

# helpers

setGeneric("asFrequencyList", function(x, positional) {
  return(standardGeneric("asFrequencyList"));
})

setMethod("asFrequencyList", c("Tabulated", "character"), function(x, positional) {
#      as(x, "FrequencyList", positional);
  from <- x;
  if (! positional %in% lpositional(from)) {
    stop("Unknown positional attribute");
  }
  freq <- table(from[,positional]);
  return(frequencyList(freq));
});

setMethod("asFrequencyList", c("FullText"), function(x) {
      as(x, "FrequencyList");
});

setMethod("asFrequencyList", c("LexicalTable"), function(x) {
      as(x, "FrequencyList");
});

