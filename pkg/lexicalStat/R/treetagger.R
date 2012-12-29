##
## TODO Voir la fin du fichier (partie ancienne) à partir de extract.field.in.treetagger.struct
##

#library(koRpus);
#read.treetagger <- function() {
#    kRp.POS.tags(lang="fr")
#    x <- treetag("~/resources/dictionnaires/PetitLarousse1905/Source/12Fichiers_relus_interrogeables/UV_definitif.xml", treetagger="~/resources/tagger/treetagger/mac_intel4/cmd/tree-tagger-french-utf8", lang="en")
#}

##
 #
 # Read a file produced by treetagger and convert it into a dataframe of class "corpus".
 #
 # If XML tags are found in the file, they are represented "ala CWB" (see documentation
 # of class "corpus"). In extra column of the data frame: each element has it own column
 # and each start/end tag pair is representend as an id in this column
 #
 # For instance, the following file :
 #
 ################################################
 # <?xml version="1.0" encoding="UTF-8"?>
 # <EntryFree>
 # <form>
 # baba    ADJ     baba
 # </form>
 # <def>
 # Gâteau  NOM     gâteau
 # dans    PRP     dans
 # lequel  PRO:REL lequel
 # il      PRO:PER il
 # entre   VER:pres        entrer
 # du      PRP:det du
 # cédrat  NOM     cédrat
 # ,       PUN     ,
 # du      PRP:det du
 # raisin  NOM     raisin
 # de      PRP     de
 # Corinthe        NAM     Corinthe
 # et      KON     et
 # du      PRP:det du
 # rhum    NOM     rhum
 # ou      KON     ou
 # du      PRP:det du
 # kirsch  NOM     kirsch
 # .       SENT    .
 # </def>
 # </EntryFree>
 # <EntryFree>
 # <form>
 # babeurre        NOM     babeurre
 # </form>
 # <def>
 # Liquide NAM     <unknown>
 # séreux  ADJ     séreux
 # qui     PRO:REL qui
 # reste   VER:pres        rester
 # après   PRP     après
 # le      DET:ART le
 # barattage       NOM     barattage
 # de      PRP     de
 # la      DET:ART le
 # crème   NOM     crème
 # .       SENT    .
 # </def>
 ################################################
 #
 # Results in the following data frame :
 #
 ################################################
 #               V1       V2          V3 EntryFree form def cit
 # 3           baba      ADJ        baba         1    1   0   0
 # 6         Gâteau      NOM      gâteau         1    0   1   0
 # 7           dans      PRP        dans         1    0   1   0
 # 8         lequel  PRO:REL      lequel         1    0   1   0
 # 9             il  PRO:PER          il         1    0   1   0
 # 10         entre VER:pres      entrer         1    0   1   0
 # 11            du  PRP:det          du         1    0   1   0
 # 12        cédrat      NOM      cédrat         1    0   1   0
 # 13             ,      PUN           ,         1    0   1   0
 # 14            du  PRP:det          du         1    0   1   0
 # 15        raisin      NOM      raisin         1    0   1   0
 # 16            de      PRP          de         1    0   1   0
 # 17      Corinthe      NAM    Corinthe         1    0   1   0
 # 18            et      KON          et         1    0   1   0
 # 19            du  PRP:det          du         1    0   1   0
 # 20          rhum      NOM        rhum         1    0   1   0
 # 21            ou      KON          ou         1    0   1   0
 # 22            du  PRP:det          du         1    0   1   0
 # 23        kirsch      NOM      kirsch         1    0   1   0
 # 24             .     SENT           .         1    0   1   0
 # 29      babeurre      NOM    babeurre         2    2   0   0
 # 32       Liquide      NAM   <unknown>         2    0   2   0
 # 33        séreux      ADJ      séreux         2    0   2   0
 # 34           qui  PRO:REL         qui         2    0   2   0
 # 35         reste VER:pres      rester         2    0   2   0
 # 36         après      PRP       après         2    0   2   0
 # 37            le  DET:ART          le         2    0   2   0
 # 38     barattage      NOM   barattage         2    0   2   0
 # 39            de      PRP          de         2    0   2   0
 # 40            la  DET:ART          le         2    0   2   0
 # 41         crème      NOM       crème         2    0   2   0
 # 42             .     SENT           .         2    0   2   0
 ################################################
 # 
 # 
 #
 ##

read.treetagger <- function(file) {
  debug <- TRUE;
  if (debug) print("Read lines");
  x <- readLines(file);

  if (debug) print("Removing XML header if any");
  if (grepl("^<\\?xml", x[1])) {
    x <- x[-1];
  }

  if (debug) print("Is markup ?");
  is.markup <- grepl("^<", x);

  if (debug) print("Field length ?");
  fields <- strsplit(x, "\t", fixed=TRUE);
  if (debug) print(fields[1:20]);

  if (debug) print("Is field ?");
  is.field <- sapply(fields, length) == 3;

  neither.markup.nor.field <- (!is.markup) & (!is.field);
  if (any(neither.markup.nor.field)) {
#  print(which(neither.markup.nor.field))
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

  unique.end.tag <- unique(tags.name.end.tag);
  unique.start.tag <- unique(tags.name.start.tag);

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
    if (debug) print(paste("... ... itérations:", length(index.start)));
    start.and.end <- mapply(`:`, index.start, index.end)
    size.of.chunk <- sapply(start.and.end, length);
    index <- rep(0:(length(start.and.end)-1), times=size.of.chunk)
    markup.matrix[unlist(start.and.end), t] <- index;
  }

  corpus <- corpus[-which(is.markup),];
  markup.matrix <- markup.matrix[-which(is.markup),];
  corpus <- cbind(corpus, markup.matrix);

  class(corpus) <- c(class(corpus), "corpus");

  return(corpus);
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
