# source("R/read.dictionary.R")
# a <- read.text("data/text.txt")
# View(table(unlist(sapply(a, function(x) x["mb"]))))

# x <- lapply(a, function(x) x[c("mb", "ge")])
# pair <- lapply(x, function(x) paste(x$mb, x$ge, sep="--"))
# pair <- table(unlist(pair))
# View(pair)

## classé par glose
#x <- lapply(a, function(x) x[c("mb", "ge")])
#pair <- lapply(x, function(x) paste(x$ge, x$mb, sep="--"))
#pair <- table(unlist(pair))
#pair <- pair[ordered(names(pair))]
#View(pair)

read.text <- function(url) {
  debug <- F
   field_merged <- .get.merged.lines(url);
#	id <- .get.id(url, "id")
   field_merged <- .remove.header(field_merged, "ref");
   entry <- .group.by.key(field_merged, "ref");
   if (debug) print ("entry:")
    if (debug) print(entry[1:2])

   # extract field name
   field_name <- lapply(entry, substr, start=2, stop=3)
if (debug) print("field_name")   
if (debug) print(field_name[1:2])
   

   len <- length(entry)
   if (debug) len <- 2
for (i in 1:len) {
  if (debug) print("itération")
  if (debug) print(i)
  
  
  is.splittable <- grep(x=field_name[[i]], pattern="^(tx|mb|ge|ps)")
		
    if (debug) print("is.splittable")
		if (debug) print(is.splittable)

    x <- entry[[i]]
  names(x) <- field_name[[i]];
  if (debug) print("x")
		if (debug) print(x)
		x <- substr(x, start=5, stop=nchar(x, type="c"));
  if (debug) print("x après suppression des premiers caractères")
  if (debug) print(x)
  x <- as.list(x);
  if (debug) print("x as list")
  if (debug) print(x)
  
		y <- lapply(x[is.splittable], strsplit, split=" +")
  if (debug) print("y")
  if (debug) print(y)
  for (z in 1:length(is.splittable)) {
      x[is.splittable[z]] <- y[[z]]
    }
    
     entry[[i]] <- x
   }
	return(entry)
}

read.dictionary <- function (url) {
  
  field_merged <- .get.merged.lines(url);
  
  field_merged <- .remove.header(field_merged, "lx");
  
  entry <- .group.by.key(field_merged, "lx");
  
  # extract field name
  field_name <- lapply(entry, substr, start=2, stop=3)
  
  # extract field value
  for (i in 1:length(entry)) {
    x <- entry[[i]];
    x <- substr(x, start=5, stop=nchar(x, type="c"));
    entry[[i]] <- x
  }
  
  # add field names to value
  for (i in 1:length(entry)) {
    x <- entry[[i]];
    names(x) <- field_name[[i]];
    entry[[i]] <- x
  }
  return(entry)
}

# read and collapse multi-line fields into one line.
.get.merged.lines <- function(url) {
   l <- readLines(url)
   field_index <- grep(pattern = "^\\\\", x=l, perl=TRUE)
   length_field <- c(field_index[2:length(field_index)] - field_index[1:(length(field_index)-1)], (length(l)+1) - field_index[length(field_index)]);
   field_id <- rep(1:length(field_index), length_field)
   field <- split(l, field_id)
   field_merged <- sapply(field, paste, collapse=" ");
	return(field_merged);
}

.remove.header <- function(field, key) {
	# group fields by entry.
	key <- paste("^\\\\", key, sep="");
	while(! grepl(pattern=key, x=field[1], perl=T)) {
  	 field <- field[-1];
	}
	return(field);
}


.get.id <- function(data, key) {
	key <- paste("^\\\\", key, sep="");
	id_index <- grep(pattern = key, x=data, perl=TRUE)
	if (length(id_index) != 1) {
		stop(paste("more than one id (", length(id_index), " ", key,")"));
	}
	return(data[id_index]);
}

.group.by.key <- function(data, key) {
	key <- paste("^\\\\", key, sep="");
	entry_index <- grep(pattern = key, x=data, perl=TRUE)
	length_entry <- c(entry_index[2:length(entry_index)] - entry_index[1:(length(entry_index)-1)], (length(data)+1) - entry_index[length(entry_index)]);
	entry_id <- rep(1:length(entry_index), length_entry)
	data <- split(data, entry_id)
	return(data);
}

