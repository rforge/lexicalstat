##############################################################
setMethod("kwic", "FullTextList", function(corpus, pattern, left=20, right=20) {
  lines.names <- names(corpus);
  lines <- lapply(corpus, function(x) paste(x, collapse=" "));
  lines <- as.character(lines);
  names(lines) <- lines.names;
  return(.conc(lines, pattern, left, right));
});

##############################################################
# TODO
setMethod("kwic", "TabulatedDataFrame", function(corpus, pattern, left=5, right=5) {
  stop("not implemented yet");
});

.conc <- function(lines, pattern, left, right) {
  lines.names <- names(lines);
  if (is.null(lines.names)) {
    lines.names <- paste("line", 1:length(lines));
  }

  before <- paste(rep(" ", left+1), collapse="");
  after <- paste(rep(" ", right+1), collapse="");
  lines <- paste(before, lines, after, sep="");

  pos.by.lines <- gregexpr(pattern=pattern, text=lines);
  is.found.in.lines <- sapply(pos.by.lines, function(pos.in.line) { pos.in.line[1] != -1 });
  index.lines <- which(is.found.in.lines);

  if (length(index.lines) == 0) return(NULL);

  pos.by.lines <- pos.by.lines[index.lines];
  lines.names <- lines.names[index.lines];
  lines <- lines[index.lines];
  nbr.occ.by.lines <- sapply(pos.by.lines, length);

  conc <- vector(mode="list", length=length(lines));

  for (i in 1:length(index.lines)) {
      line <- lines[i];
      poss <- pos.by.lines[[i]];
      line.conc <- substr(x=rep(line, length(poss)), start=poss - left, stop=poss + attr(poss, "match.length") + right);
      conc[[i]] <- line.conc;
  }

  ref <- rep(lines.names, nbr.occ.by.lines);
  ref <- paste(ref, "occ.", as.numeric(unlist(sapply(nbr.occ.by.lines, function(x) seq(1, x)))));

  return(data.frame(ref=ref, concordance=unlist(conc)));
}

######################################### TODO Following functions to be tested/debugged/reused?

###
### To be debugged
###

word.conc <- function(tokens.by.part, pattern, left, right) {
  debug <- TRUE;

  lines.names <- names(tokens.by.part);
  if (is.null(lines.names)) {
    lines.names <- 1:length(tokens.by.part);
  }

  conc <- matrix("", nrow=0, ncol = left + right + 1);
  for (i in 1:length(tokens.by.part)) {
     line <- tokens.by.part[[i]];
     line.name <- lines.names[i];

     where.found <- gregexpr(pattern=pattern, text=line);

     is.in.line <- sapply(where.found, function(x) { x[1] != -1 })
     index.line <- which(is.in.line);
     if (debug) print(paste("Number of match:", length(index.line)))

     if (length(index.line) > 0) {
       conc.line <- matrix("", nrow=length(index.line), ncol=left + right + 1);
       conc.line[, left+1] <- line[index.line];
       rownames(conc.line) <- paste(line.name, 1:length(index.line), sep=":");
       for (j in 1:length(index.line)) {
         word.index <- index.line[j];

         from <- max(1, word.index - left);
         index.left <- from:(word.index - 1);;

         to <- min(word.index + right, length(line));
         index.right <- (word.index + 1):to;

         #print(j);
         #print(from);
         #print(to);
         #print(word.index);
         #print(index.left);
         #print(index.right);

         if(word.index != 1) {
           conc.line[j, (left-length(index.left)):left] <- line[index.left];
         }
         if(word.index != length(line)) {
           conc.line[j, (left+2):(left+1+length(index.right))] <- line[index.right];
         }
       }
       conc <- rbind(conc, conc.line);
     }
  }
  return(conc);
}

text.conc <- function(lines, pattern, left, right) {
  lines.names <- names(lines);
  if (is.null(lines.names)) {
    lines.names <- 1:length(lines);
  }
  conc <- list();
  for (i in 1:length(lines)) {
    line <- lines[[i]];
    line.name <- lines.names[i];

    where.found <- gregexpr(pattern=pattern, text=line);
    where.found <- where.found[[1]];
    if (where.found[1] != -1) {
      nfound <- length(where.found);
      #print(where.found);
      #print(attr(where.found, "match.length"));
      match.length <- attributes(where.found)$match.length;
      # attr(where.found, "match.length"):
      #print(match.length)

      prefix.length <- min(where.found - left);
      if (prefix.length < 1) {
        prefix.length <- abs(prefix.length);
        prefix <- paste(character(prefix.length + 1), collapse=" ");
        line <- paste(prefix, line, sep="");
      } else {
        prefix.length <- 0;
      }

      postfix.length <- max(prefix.length, where.found + match.length + right);
      if (postfix.length > nchar(line)) {
        postfix.length <- postfix.length - nchar(line);
        postfix <- paste(character(postfix.length + 1), collapse=" ");
        line <- paste(line, postfix, sep="");
      }

      ss <- substr(
          rep(line, nfound),
          where.found + prefix.length - left,
          where.found + prefix.length + match.length + right
          );

      #ss[where.found - left < 1] <-
      #  paste(
      #      rep(" " abs(where.found - left)),
      #      ss[where.found - left < 1],
      #      sep=""
      #      )
      #ss[where.found - left < 1] <-
      #  paste(
      #      rep(" " abs(where.found - left)),
      #      ss[where.found - left < 1],
      #      sep=""
      #      )

    conc <- append(conc, ss);
    }
  }
  return(as.character(conc));
}
