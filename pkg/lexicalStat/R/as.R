############################################################
##
## As Tabulated
##
############################################################

setAs("FullText", "Tabulated", function(from, to) {
  word <- unlist(from);
  df <- data.frame(word=as.factor(word));

  len <- sapply(from, length);
  id <- 0:(length(from) - 1);
  part <- rep(id, times=len);
  df[, "part"] <- as.factor(part);

  res <- tabulated(df, 1);
  return(res);
});

##############################################################
setGeneric("as.Tabulated", function(corpus) {
  return(standardGeneric("as.Tabulated"));
})

##############################################################
setMethod("as.Tabulated", "FullText", function(corpus) {
  as(corpus, "Tabulated");
});

##############################################################
setMethod("as.Tabulated", "Tabulated", function(corpus) {
  return(corpus);
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

##############################################################
setGeneric("as.FullText", function(x, positional, structural) {
  return(standardGeneric("as.FullText"));
})

##############################################################
setMethod("as.FullText", c("Tabulated", "character", "character"), function(x, positional, structural) {
#      as(x, "FullText", positional, structural);
  if (is.null(positional)) {
    stop("positional cannot be NULL");
  }
  if (is.null(structural)) {
    stop("structural cannot be NULL");
  }
# TODO check if structural and positional exist
  l <- split(x[,positional], x[,structural])
  l <- lapply(l, as.character);
  obj <- fullText(l);
  return(obj);
});

##############################################################
setMethod("as.FullText", c("FullText"), function(x) {
  return(x);
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
  if (debug) print("[list2lexical.table.sparse] ...types and tokens...");

  tokens <- unlist(from);
  types <- unique(tokens);

  if (debug) print("[list2lexical.table.sparse] ...tables and tables length...");
  #part.lengths <- sapply(from, length);
  part.tables <- lapply(from, table);
  part.table.lengths <- sapply(part.tables, length);

  if (debug) print("[list2lexical.table.sparse] ...i...");
  i <- unlist(sapply(part.tables, function(t) names(t)));
  i <- match(i, types);
  if (debug) print("[list2lexical.table.sparse] ...j...");
  j <- rep(1:length(from), part.table.lengths);
  if (debug) print("[list2lexical.table.sparse] ...v...");
  v <- as.vector(unlist(sapply(part.tables, as.numeric)));

  if (debug) print("[list2lexical.table.sparse] ...i...");
  if (debug) print(i);
  if (debug) print("[list2lexical.table.sparse] ...j...");
  if (debug) print(j);
  if (debug) print("[list2lexical.table.sparse] ...v...");
  if (debug) print(v);
  if (debug) print("[list2lexical.table.sparse] ...creating matrix...");
  #m <- spMatrix(nrow=length(from), ncol=length(types), i=i, j=j, x=v);
  m <- sparseMatrix(i=i, j=j, x=v);
  rownames(m) <- types;
  colnames(m) <- names(from);
  return(lexicalTable(m));
});

# setAs("LexicalTable", "Tabulate", function(from, to, positional, structural) {
#   if (! structural %in% attr(from, "structural")) {
#     stop("structural attribute not known");
#   }
#   positional.factor <- as.factor(from[, positional]);
#   structural.factor <- as.factor(from[, structural]);
#   types <- levels(positional.factor);
#   parts <- levels(structural.factor);
# 
#   f <- count(data.frame(as.numeric(positional.factor), as.numeric(structural.factor)));
#   i <- f[,1];
#   j <- f[,2];
# 
#   m <- sparseMatrix(i=i, j=j, x=f[,3]);
#   rownames(m) <- types;
#   colnames(m) <- parts;
# 
#   return(lexicalTable(m));
# });

# helpers

##############################################################
setGeneric("as.LexicalTable", function(x, positional, structural) {
  return(standardGeneric("as.LexicalTable"));
})

##############################################################
setMethod("as.LexicalTable", c("Tabulated"), function(x, positional, structural) {
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

##############################################################
setMethod("as.LexicalTable", c("LexicalTable"), function(x) {
  return(x);
});

##############################################################
setMethod("as.LexicalTable", c("FullText"), function(x) {
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

##############################################################
setGeneric("as.FrequencyList", function(x, positional) {
  return(standardGeneric("as.FrequencyList"));
})

##############################################################
setMethod("as.FrequencyList", c("Tabulated", "character"), function(x, positional) {
#      as(x, "FrequencyList", positional);
  from <- x;
  if (! positional %in% lpositional(from)) {
    stop("Unknown positional attribute");
  }
  freq <- table(from[,positional]);
  return(frequencyList(freq));
});

##############################################################
setMethod("as.FrequencyList", c("FullText"), function(x) {
      as(x, "FrequencyList");
});

##############################################################
setMethod("as.FrequencyList", c("LexicalTable"), function(x) {
      as(x, "FrequencyList");
});

##############################################################
setMethod("as.FrequencyList", "FrequencyList", function(x) {
  return(x);
});

