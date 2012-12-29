## 
## 
## merge.token.lists <- function(corpus, factor) {
##   if (length(corpus) != length(factor)) {
##     stop("Factor and corpus (list of tokens vectors) must have the same length");
##   }
##   return(tapply(corpus, factor, unlist));
## }
## 
## 
## .add.to.lexical.table <- function(lexical.table, obs, row) {
##   if (! is.matrix(lexical.table)) stop("lexical.table is not a matrix");
##   if (! is.numeric(lexical.table)) stop("lexical.table is not a numeric matrix");
##   if (! is.character(obs)) stop(paste("obs is not a character vector:", obs));
## 
## # remove "" var.
##   if (any("" == obs)) {
##     obs <- obs[-which("" == obs)];
##   }
## 
##   freq <- table(obs);
##   var.names <- names(freq);
##   var.freq <- as.numeric(freq);
## 
##   nbr.obs <- nrow(lexical.table);
## 
##   lexical.table[row, var.names] <- var.freq;
##   return(lexical.table);
## }
## 
## list2lexical.table.sparse <- function(l) {
##   print("[list2lexical.table.sparse] checking argument...");
##   all.character <- sapply(l, is.character);
##   if (!all(all.character)) stop("all element of the list must be character vector of token");
##   print("[list2lexical.table.sparse] ...forms and tokens...");
##   tokens <- unlist(l);
##   forms <- unique(tokens);
## 
##   print("[list2lexical.table.sparse] ...tables and tables length...");
##   #part.lengths <- sapply(l, length);
##   part.tables <- lapply(l, table);
##   part.table.lengths <- sapply(part.tables, length); 
## 
##   print("[list2lexical.table.sparse] ...i...");
##   i <- rep(1:length(l), part.table.lengths);
##   print("[list2lexical.table.sparse] ...j...");
##   j <- unlist(sapply(part.tables, function(t) which(forms %in% names(t))));
##   print("[list2lexical.table.sparse] ...v...");
##   v <- unlist(sapply(part.tables, as.numeric));
## 
##   print("[list2lexical.table.sparse] ...creating matrix...");
##   m <- spMatrix(nrow=length(l), ncol=length(forms), i=i, j=j, x=v);
##   rownames(m) <- names(l);
##   colnames(m) <- forms;
##   print("[list2lexical.table.sparse] ...ok.");
##   return(m);
## }
## 
## ##
## ## Convert a list of character vectors representing modalities into a
## ## contingency table.
## ##
## ## char "" are removed.
## ##
## list2lexical.table <- function(l) {
##   forms <- unique(unlist(l));
##   l.t <- matrix(0, length(l), length(forms));
## 
##   colnames(l.t) <- forms;
##   if(!is.null(names(l))) {
##     rownames(l.t) <- names(l);
##   } else {
##     rownames(l.t) <- 1:length(l);
##   }
##   
##   for (i in 1:length(l)) {
##     l.t <- .add.to.lexical.table(l.t, l[[i]], i);
##   }
## 
##   return(l.t);
## }
## 
## # add.to.lexical.table <- function(lexical.table, obs) {
## #   if (! is.data.frame(lexical.table)) stop("lexical.table is not a data.frame");
## #   if (! is.character(obs)) stop(paste("obs is not a character vector:", obs));
## # 
## # # remove "" var.
## #   if (any("" == obs)) {
## #     obs <- obs[-which("" == obs)];
## #   }
## # 
## #   freq <- table(obs);
## #   var.names <- names(freq);
## #   var.freq <- as.numeric(freq);
## # 
## #   nbr.obs <- nrow(lexical.table);
## # 
## #   existing.vars <- colnames(lexical.table);
## #   is.existing.vars <- var.names %in% existing.vars;
## # 
## #   if (sum(!is.existing.vars) > 0) {
## #     new.vars <- var.names[!is.existing.vars];
## #     new.cols <- matrix(0, nrow=nbr.obs, ncol=length(new.vars));
## #     colnames(new.cols) <- new.vars;
## #     lexical.table <- cbind(lexical.table, new.cols);
## #   }
## # 
## #   new.row <- matrix(0, nrow=1, ncol=ncol(lexical.table));
## #   colnames(new.row) <- colnames(lexical.table);
## #   lexical.table <- rbind(lexical.table, new.row);
## #   lexical.table[nbr.obs+1, var.names] <- var.freq;
## #   dim(lexical.table);
## #   return(lexical.table);
## # }
## # 
## # ##
## # ## Convert a list of character vectors representing modalities into a
## # ## contingency table.
## # ##
## # ## char "" are removed.
## # ##
## # list2lexical.table <- function(l) {
## #   l.t <- data.frame();
## #   for (i in l) {
## #     l.t <- add.to.lexical.table(l.t, i);
## #   }
## #   if(!is.null(names(l))) {
## #     rownames(l.t) <- names(l);
## #   } else {
## #     rownames(l.t) <- 1:length(l);
## #   }
## #   return(l.t);
## # }
## 
## # ##
## # ## "" are not included.
## # ## 
## #
## # ## Old implementation (slow);
## #
## # list2lexical.table.old <- function(l) {
## #   V <- unique(unlist(l));
## #   l.t <- matrix(0, nrow=length(l), ncol=length(V));
## #   colnames(l.t) <- V;
## #   if(!is.null(names(l))) {
## #     rownames(l.t) <- names(l);
## #   }
## # 
## #   #utiliser table() pour avoir , dans chaque observation, le total par forme:
## #   #for (i in 1:length(l)) {
## #   #  sub.freq <- table(l[[i]]);
## #   #  dif.freq <- unique(sub.frequ);
## #   #  for(f in dif.freq) {
## #   #    l.t [i, names(table)[table == f] ] <- f;
## #   #  }
## #   #}
## # 
## #   for (i in 1:length(l)) {
## #     for (j in 1:length(l[[i]])) {
## #       var <- l[[i]][j];
## #       if (var == "") next;
## #       l.t[i, var] <- l.t[i, var] + 1;
## #     }
## #   }
## #   return(l.t);
## # }
## 
