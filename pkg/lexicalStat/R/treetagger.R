##
## TODO Voir la fin du fichier (partie ancienne) a partir de extract.field.in.treetagger.struct
##

#library(koRpus);
#read.treetagger <- function() {
#    kRp.POS.tags(lang="fr")
#    x <- treetag("~/resources/dictionnaires/PetitLarousse1905/Source/12Fichiers_relus_interrogeables/UV_definitif.xml", treetagger="~/resources/tagger/treetagger/mac_intel4/cmd/tree-tagger-french-utf8", lang="en")
#}

##############################################################
read.treetagger <- function(file, contains.xml=TRUE, discard.xml=FALSE) {
  if (!file.exists(file)) {
    stop("cannot read or access file");
  }

  debug <- FALSE;
  if (debug) print("Read lines");
  x <- readLines(file);

  if (contains.xml) {
    if (debug) print("Removing XML header if any");
    if (grepl("^<\\?xml", x[1])) {
      x <- x[-1];
    }
    if (debug) print("Is markup ?");
    is.markup <- grepl("^<", x);

    if (sum(is.markup) == 0) {
      stop("'contains.xml=TRUE' but no XML found in the file");
    }
  } else {
    is.markup <- FALSE;
  }

  if (debug) print("Field length ?");
  fields <- strsplit(x, "\t", fixed=TRUE);
  if (debug) print(fields[1:20]);

  if (debug) print("Is field ?");
  is.field <- sapply(fields, length) == 3;

  neither.markup.nor.field <- (!is.markup) & (!is.field);
  if (any(neither.markup.nor.field)) {
    stop(paste("Some lines are strange:", paste(x[neither.markup.nor.field], collapse="\n"), sep="\n"));
  }

##
## Create the final data frame
##

  all.three <- lapply(fields, function(x) if (length(x) == 3) x else if (length(x) == 1) c(x, "", "") else stop("wrong length"));
  if (debug) print(all.three[1:20]);
  corpus <- as.data.frame(matrix(unlist(all.three), byrow=TRUE, nrow=length(fields)));
  colnames(corpus) <- c("word", "pos", "lemma");

##
## Markup
##

  positional.attributes <- colnames(corpus);
  structural.attributes <- character();
  if (contains.xml) {
    if(!discard.xml) {
      markup.matrix <- .get.markup.matrix(x, is.markup, debug);
      structural.attributes <- colnames(markup.matrix);
      corpus <- cbind(corpus, markup.matrix);
    }
    corpus <- corpus[-which(is.markup),];
  }

  for (s in structural.attributes) {
    corpus[,s] <- as.factor(corpus[,s]);
  }

  corpus <- tabulated(corpus, length(structural.attributes));
  return(corpus);
}

##
 # ----------------------------------------------------
 #
 # Private function for dealing with XML markup
 #
 # ----------------------------------------------------
 ##
.get.markup.matrix <- function(x, is.markup, debug) {
  markup <- x[is.markup];

#
# Check that each line starting with "<" contains one and only one tag.
#

  if (debug) print("Is well formed ?");
  is.well.formed <- sapply(gregexpr("^</?(.+)>$", markup), `==`, 1);
  if (any(!is.well.formed)) {
    stop(paste("some markup is not well formed (one tag per line):", paste(markup[!is.well.formed], collapse="\n"), sep="\n"))
  }

#
# start tag vs end tag
#

  if (debug) print("Is end tag ?");
  is.end.tag <- grepl("^</", markup);

  tags.name.end.tag <- substr(markup[is.end.tag], 3, nchar(markup[is.end.tag])-1);
  tags.name.start.tag <- substr(markup[!is.end.tag], 2, nchar(markup[!is.end.tag])-1);

  unique.end.tag <- sort(unique(tags.name.end.tag));
  unique.start.tag <- sort(unique(tags.name.start.tag));

  if (debug) print(unique.end.tag);
  if (debug) print(unique.start.tag);

  if (debug) print("Checking start and end tag match");
  if ((!all(unique.end.tag %in% unique.start.tag))
      &
      length(unique.start.tag) == length(unique.end.tag)) {
    stop(paste("start tag and end tag mismatch:",
	  paste(c("start tag:", unique.start.tag), collapse="\n"),
	  paste(c("end tag:", unique.end.tag), collapse="\n"),
	  sep="\n"));
  }

#
# Iterating on each tag
#

  markup.matrix <- matrix(-1, nrow=length(x), ncol=length(unique.start.tag));
  colnames(markup.matrix) <- unique.start.tag;


  if (debug) print("Iterating on each tag");
  for(t in unique.start.tag) {
    if (debug) print(paste("...", t));
    i.start <- which(!is.end.tag)[tags.name.start.tag == t];
    i.end <- which(is.end.tag)[tags.name.end.tag == t];

    index.start <- which(is.markup)[i.start];
    index.end <- which(is.markup)[i.end];
    if (debug) print(index.start[1]);
    if (debug) print(index.end[1]);
    if (length(index.start) != length(index.end)) {
      stop(paste("number of start and end tag mismatch for tag: ",
	    t,
	    " (", length(index.start), ", ", length(index.end), ")",
	    sep=""));
    }
    if (debug) print(paste("... ... iterations:", length(index.start)));
    start.and.end <- mapply(`:`, index.start, index.end, SIMPLIFY=FALSE); # If TRUE, produces a vector-matrix when length(index.start) == 1
    if(debug) print(start.and.end);
    size.of.chunk <- sapply(start.and.end, length);
    index <- rep(0:(length(start.and.end)-1), times=size.of.chunk)
    if(debug) print(index);
    markup.matrix[unlist(start.and.end), t] <- index;
  }
  return(markup.matrix);
}



## #1 = inflected form
## #2 = pos
## #3 = lemma
## extract.field.in.treetagger.struct <- function(fields.by.line.by.chunk, field.nbr) {
##   sapply(fields.by.line.by.chunk, function(lines) {
##       lapply(lines, function(fields) {
##         if (length(fields) == 3) {
##         return(fields[field.nbr]);
## #return(paste(fields[field.nbr], substr(fields[2], 1, 3), sep="-"))
##         } else {
##         return("")
##         }
##         })});
## }
## 
## extract.word.on.pos.in.treetagger.struct <- function(fields.by.line.by.chunk, pos.regexp="(^NOM|^ADJ|^VER)") {
##   lapply(fields.by.line.by.chunk, function(lines) {
##       cat <- sapply(lines, `[`, 2);
##       index <- grepl(pos.regexp, cat);
##       return(lines[index]);
##       });
## }
## 
## 
## ## remove token with "unknown" lemma
## remove.unknown.in.treetagger.struct <- function(fields.by.line.by.chunk, unknown.str="<unknown>") {
##   return(lapply(fields.by.line.by.chunk, function(lines) {
##         if (length(lines) == 0) return(list());
##         index <- sapply(lines, function(token) {length(token) == 3 && token[3] != unknown.str});
##         #print(index);
## 	return(lines[ index ]);
## 	}));
## }
## 
## ## replace "unknown" lemma with the lower case inflected form
## repair.unknown.in.treetagger.struct <- function(fields.by.line.by.chunk, unknown.str="<unknown>") {
##   return(lapply(fields.by.line.by.chunk, function(lines) {
##         lapply(lines, function (fields) {
##           if (length(fields) == 3 & fields[3] != "" & fields[3]  == unknown.str) {
##           fields[3] <- tolower(fields[1]);
##           }
##           return(fields);
##           })}));
## }
## 
## ## An xml document containing tree tagger chunk (3-columns lines) in
## ## the given element
## get.treetagger.struct.in.xml.element <- function(filename, element) {
## #els <- elementcontent2list.xpath(filename, element);
##   print("extracting content of the element");
##   els <- elementcontent2list.sax(filename, element);
##   print("extracting tree tagger field");
##   treetagger.struct <- characters2treetagger.struct(els);
##   return(treetagger.struct);
## }
## 
## ## Convert a list of chunk in a list where each chunk element contains a list
## ## of words as character vector of fields.
## # characters2treetagger.struct(c("un\tdeux\ttrois\nquatre\tcinq\tsix", "sept\thuit\tneuf\ndix\tonze\tdouze"))
## characters2treetagger.struct <- function(treetagger.chunks) {
##   if (!is.character(treetagger.chunks)) {
##     stop("treetagger.chunks must be character vector");
##   }
##   lines.by.chunk <- strsplit(treetagger.chunks, "\n");
##   lines.by.chunk <- lapply(lines.by.chunk, function(x) x[x != ""]);
## #print(lines.by.chunk[1:100]);
##   fields.by.line.by.chunk <- lapply(lines.by.chunk, function(lines) {
##       fields.by.line <- strsplit(lines, "\t");
##       });
##   return(fields.by.line.by.chunk);
## }
## 
## 
