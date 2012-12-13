#1 = inflected form
#2 = pos
#3 = lemma
extract.field.in.treetagger.struct <- function(fields.by.line.by.chunk, field.nbr) {
  sapply(fields.by.line.by.chunk, function(lines) {
      lapply(lines, function(fields) {
        if (length(fields) == 3) {
        return(fields[field.nbr]);
#return(paste(fields[field.nbr], substr(fields[2], 1, 3), sep="-"))
        } else {
        return("")
        }
        })});
}

extract.word.on.pos.in.treetagger.struct <- function(fields.by.line.by.chunk, pos.regexp="(^NOM|^ADJ|^VER)") {
  lapply(fields.by.line.by.chunk, function(lines) {
      cat <- sapply(lines, `[`, 2);
      index <- grepl(pos.regexp, cat);
      return(lines[index]);
      });
}


## remove token with "unknown" lemma
remove.unknown.in.treetagger.struct <- function(fields.by.line.by.chunk, unknown.str="<unknown>") {
  return(lapply(fields.by.line.by.chunk, function(lines) {
        if (length(lines) == 0) return(list());
        index <- sapply(lines, function(token) {length(token) == 3 && token[3] != unknown.str});
        #print(index);
	return(lines[ index ]);
	}));
}

## replace "unknown" lemma with the lower case inflected form
repair.unknown.in.treetagger.struct <- function(fields.by.line.by.chunk, unknown.str="<unknown>") {
  return(lapply(fields.by.line.by.chunk, function(lines) {
        lapply(lines, function (fields) {
          if (length(fields) == 3 & fields[3] != "" & fields[3]  == unknown.str) {
          fields[3] <- tolower(fields[1]);
          }
          return(fields);
          })}));
}

## An xml document containing tree tagger chunk (3-columns lines) in
## the given element
get.treetagger.struct.in.xml.element <- function(filename, element) {
#els <- elementcontent2list.xpath(filename, element);
  print("extracting content of the element");
  els <- elementcontent2list.sax(filename, element);
  print("extracting tree tagger field");
  treetagger.struct <- characters2treetagger.struct(els);
  return(treetagger.struct);
}

## Convert a list of chunk in a list where each chunk element contains a list
## of words as character vector of fields.
# characters2treetagger.struct(c("un\tdeux\ttrois\nquatre\tcinq\tsix", "sept\thuit\tneuf\ndix\tonze\tdouze"))
characters2treetagger.struct <- function(treetagger.chunks) {
  if (!is.character(treetagger.chunks)) {
    stop("treetagger.chunks must be character vector");
  }
  lines.by.chunk <- strsplit(treetagger.chunks, "\n");
  lines.by.chunk <- lapply(lines.by.chunk, function(x) x[x != ""]);
#print(lines.by.chunk[1:100]);
  fields.by.line.by.chunk <- lapply(lines.by.chunk, function(lines) {
      fields.by.line <- strsplit(lines, "\t");
      });
  return(fields.by.line.by.chunk);
}


