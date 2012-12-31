# TODO : dans l'impression : mettre option si "+" ou "-".


setMethod("print", signature(x="WordAssociation"), function(x, from=1, to=50, threshold=NULL, types=NULL, parts=NULL, file="", sort.by=NULL, append=FALSE, ...) {

  printable <- .get.printable(x, from, to, threshold, types, parts, sort.by);

  msg <- paste("Printing association measure for", length(printable), "part(s);");
  if (!is.null(threshold)) {
    msg <- paste(msg, "threshold:", threshold, "\n");
  } else {
    msg <- paste(msg, "from:", from, "to:", to, "\n");
  }
  cat(msg, file=file, append=append);

  indicators <- indicator.name(x);

  cat(paste("Corpus size:", N(x)[1], "\n"), file=file, append=append);
  cat(sprintf("-----------------------------------------------------\n"), file=file, append=append);
  cat(sprintf("%-20s | %8s | %8s \n", "word", "sub freq", "tot freq"), file=file, append=append);
  for (i in indicators) {
    cat(sprintf("| %8s ", i), file=file, append=append);
  }
  cat(sprintf("\n"), file=file, append=append);
  cat(sprintf("-----------------------------------------------------\n"), file=file, append=append);

  for (i_part in 1:length(printable)) {
    part <- printable[[i_part]];
    cat(".....................................................\n", file=file, append=append);
    cat(paste("Part name:", part$part[1], "\n"), file=file, append=append);
    cat(paste("Part size:", part$n[1], "tokens.", "\n"), file=file, append=append);
    cat(paste("Positive specificities:", sum(part[,2] > 0), "\n"), file=file, append=append);
    cat(paste("Negative specificities:", sum(part[,2] < 0), "\n"), file=file, append=append);
    if (nrow(part) > 0) {
      for (i_word in 1:nrow(part)) {
        word_name <- part[i_word, "forms"];
        word_sub_freq <- part[i_word, "k"];
        word_tot_freq <- part[i_word, "K"];
        cat(sprintf("%-20s | %8.0f | %8.0f ", word_name, word_sub_freq, word_tot_freq), file=file, append=append);
	for (i in indicators) {
	  cat(sprintf("| %8.2f ", part[i_word, i]), file=file, append=append);
	}
	cat(sprintf("\n"), file=file, append=append);
      }
    }
  }
});


.get.printable <- function(x, from, to, threshold, types=NULL, parts=NULL, sort.by) {

  if(!class(x) == "WordAssociation") stop("x must be of class WordAssociation");

  if (is.null(threshold)) {
    if (is.null(from) | is.null(to)) {
      stop("either 'threshold' or 'from' and 'to' options must be given");
    }
    if ((!is.numeric(from)) | (!is.numeric(to))) {
      stop("both 'from' and 'to' must be numeric");
    }
    if (from >= to) {
      stop("'to' must be greater than 'from'");
    }
  } else {
    if (!is.numeric(threshold)) {
      stop("threshold must be numeric");
    }
    if (threshold <= 0) {
      stop("threshold must be greater than 0");
    }
  }

  df <- data.frame(types=types(x), parts=parts(x), N=N(x), n=n(x), K=K(x), k=k(x));
  df <- cbind(df, association(x));

  if (!sort.by %in% colnames(df)) {
    stop(paste("cannot sort by '", sort.by, "': no column of that names (in: ", paste(colnames(df), collapse=" "), ")", sep=""));
  }

  if (!is.null(types)) {
    df <- df[ df$types %in% types, ];
  }

  if (!is.null(parts)) {
    df <- df[ df$parts %in% parts, ];
  }
  
  if (!is.null(threshold)) {
    df <- df[ abs(df$association) > threshold, ];
  }

  if (!is.null(sort.by)) {
    df <- df[order(df[, sort.by]),];
  }

  l <- split(df, df$parts);

  l <- lapply(l, function(x) { x <- x[from:to,] });

  return(l);
}

writeWordAssociation <- function(x, file, from=1, to=50, threshold=NULL, types=NULL, parts=NULL) {
  if(!class(x) == "specificities") stop("x must be of class specificities");
  if(is.null(file)) stop("file cannot be null");
  if (is.null(types)) types <- attr(x, "types")
  if (is.null(parts)) parts <- attr(x, "parts")
  printable <- .get.printable(x, from, to, threshold, types, parts);
  for (i in 1:length(printable)) {
      write.csv(printable[[i]], file=paste(file, "_", i, ".csv", sep=""));
  }
}
#setMethod("write", "WordAssociation", writeWordAssociation)

writeAsXML.wma <-
function(x, file, from=1, to=100, threshold=NULL, types=NULL, parts=NULL) {
  if(!class(x) == "specificities") stop("x must be of class specificities");
  if(is.null(file)) stop("file cannot be null");
  if (is.null(types)) types <- attr(x, "types")
  if (is.null(parts)) parts <- attr(x, "parts")
  printable <- .get.printable(x, from, to, threshold, types, parts);
  .saveDataFrameAsXML(as.data.frame(printable), file);
}

