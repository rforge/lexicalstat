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

  res <- tabulated(df, "word", "part");
  return(res);
});

##############################################################
#' Convert a corpus into a \code{\link{Tabulated}} corpus
#'
#' Actually, only \code{\link{FullText}} corpus can be converted into
#' \code{\link{Tabulated}}
#'
#' @param corpus a \code{\link{Corpus}}
#'
#' @return A \code{\link{Tabulated}} corpus
#' 
#' @seealso \code{\link{Corpus}} for an overview of the possible conversions
#' between corpus classes
#' 
#' @export
#' @docType methods
#' @rdname as.Tabulated-methods
#' @genericMethods
#'
#' @examples
#' data(dickensFullText)
#' as.Tabulated(dickensFullText);
setGeneric("as.Tabulated", function(corpus) {
  return(standardGeneric("as.Tabulated"));
})

##############################################################
#' @rdname as.Tabulated-methods
#' @aliases as.Tabulated,FullText-method
setMethod("as.Tabulated", "FullText", function(corpus) {
  as(corpus, "Tabulated");
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
#' Convert a corpus into a \code{\link{FullText}} corpus
#'
#' Actually, only \code{\link{Tabulated}} corpus can be converted into
#' \code{\link{FullText}}
#'
#' @param corpus a \code{\link{Corpus}}
#'
#' @return A \code{\link{FullText}} corpus
#' 
#' @seealso \code{\link{Corpus}} for an overview of the possible conversions
#' between corpus classes
#' 
#' @export
#' @docType methods
#' @rdname as.FullText-methods
#' @genericMethods
#'
#' @examples
#' data(dickensTabulated)
#' as.FullText(dickensTabulated);
setGeneric("as.FullText", function(x, positional, structural) {
  return(standardGeneric("as.FullText"));
})

##############################################################
#' @rdname as.FullText-methods
#' @aliases as.FullText,Tabulated-method
setMethod("as.FullText", c("Tabulated", "character", "character"), function(x, positional, structural) {
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
  if (debug) print("[list2lexical.table.sparse] ...types and tokens...");

  tokens <- unlist(from);
  types <- unique(tokens);

  if (debug) print("[list2lexical.table.sparse] ...tables and tables length...");
  #part.lengths <- sapply(from, length);
  part.tables <- lapply(from, table);
  part.table.lengths <- sapply(part.tables, length);

  if (debug) print("[list2lexical.table.sparse] ...i...");
  i <- unlist(sapply(part.tables, function(t) which(types %in% names(t))));
  if (debug) print("[list2lexical.table.sparse] ...j...");
  j <- rep(1:length(from), part.table.lengths);
  if (debug) print("[list2lexical.table.sparse] ...v...");
  v <- as.vector(unlist(sapply(part.tables, as.numeric)));

  if (debug) print("[list2lexical.table.sparse] ...i...");
  print(i);
  if (debug) print("[list2lexical.table.sparse] ...j...");
  print(j);
  if (debug) print("[list2lexical.table.sparse] ...v...");
  print(v);
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
#' Convert a corpus into a \code{\link{LexicalTable}} corpus
#'
#' @param corpus a \code{\link{Corpus}}
#'
#' @return A \code{\link{LexicalTable}} corpus
#' 
#' @seealso \code{\link{Corpus}} for an overview of the possible conversions
#' between corpus classes
#' 
#' @export
#' @docType methods
#' @rdname as.LexicalTable-methods
#' @genericMethods
#'
#' @examples
#' data(dickensTabulated)
#' as.LexicalTable(dickensTabulated, "word", "s");
#'
#' data(dickensFullText)
#' as.LexicalTable(dickensFullText);
setGeneric("as.LexicalTable", function(x, positional, structural) {
  return(standardGeneric("as.LexicalTable"));
})

##############################################################
#' @rdname as.LexicalTable-methods
#' @aliases as.LexicalTable,Tabulated,character,character-method
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
#' @rdname as.LexicalTable-methods
#' @aliases as.LexicalTable,FullText-method
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
#' Convert a corpus into a \code{\link{FrequencyList}} corpus
#'
#' @param corpus a \code{\link{Corpus}}
#'
#' @return A \code{\link{FrequencyList}} corpus
#' 
#' @seealso \code{\link{Corpus}} for an overview of the possible conversions
#' between corpus classes
#' 
#' @export
#' @docType methods
#' @rdname as.FrequencyList-methods
#' @genericMethods
#'
#' @examples
#' data(dickensTabulated)
#' as.FrequencyList(dickensTabulated, "word");
#'
#' data(dickensFullText)
#' as.FrequencyList(dickensFullText);
#'
#' data(dickensLexicalTable)
#' as.FrequencyList(dickensLexicalTable);
setGeneric("as.FrequencyList", function(x, positional) {
  return(standardGeneric("as.FrequencyList"));
})

##############################################################
#' @rdname as.FrequencyList-methods
#' @aliases as.FrequencyList,Tabulated,character-method
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
#' @rdname as.FrequencyList-methods
#' @aliases as.FrequencyList,FullText-method
setMethod("as.FrequencyList", c("FullText"), function(x) {
      as(x, "FrequencyList");
});

##############################################################
#' @rdname as.FrequencyList-methods
#' @aliases as.FrequencyList,LexicalTable-method
setMethod("as.FrequencyList", c("LexicalTable"), function(x) {
      as(x, "FrequencyList");
});

