library(HTMLUtils)

#phonem.inventory
#phonem.frequency

# il faudrait segmenter en mot plutôt qu'en phrase, ce que je ne peux pas faire :
# pas de marque de la segmentation de mots dans la ligne mb.
.make.morphem.ngram <- function(corpus) {
   corpus <- lapply(corpus, function(x) {x["mb"]});
   corpus <- lapply(corpus, function(x) {c("#", x[["mb"]], "#")});

   ngram <- .make.ngram(corpus);
   id <- unlist(sapply(ngram, sapply, `[`, 2))

   ngram <- unlist(lapply(ngram, sapply, paste, collapse="-"))
   ngram <- split(ngram, f=id)
   ngram <- lapply(ngram, table)
   return(ngram)
}

.make.ngram <- function(l) {
  ngram <- lapply(l,  function(x){
    if (length(x) < 3) return(list())
    r <- lapply(1:(length(x)-2), function(y) x[y:(y+2)])
    #r <- list()
    #for (i in 1:(length(x)-2)) {
    #  r[[i]] <- x[i:(i+2)]
    #}
    return(r)
  });
  return(ngram)
}
.make.phonem.ngram <- function(dictionary) {
  entry <- read.dictionary('/Users/sylvainloiseau/Recherche/Terrain/Corpus/Transcriptions/Toolbox/Dictionary.txt')
lex <- sapply(entry, function(x) x["lx"])
names(entry) <- lex

#is.noun <- sapply(entry, function(x) grepl(x=x["ps"],  pattern="^n" ))

lex_and_pos <- lapply(entry, function(x) x[c("lx", "ps")]);
lex_and_pos <- t(as.data.frame(lex_and_pos))
#is.noun <- grepl(x=lex_and_pos[, "ps"],  pattern="^n")
is.noun <- TRUE

noun <- lex_and_pos[is.noun ,1]
noun <- paste("#", noun, sep="")
noun <- paste(noun, "#", sep="")
noun <- strsplit(noun, split="")

ngram <- .make.ngram(noun);

ngram.v <- unlist(lapply(ngram, sapply, paste, collapse=""))
phonem <- substr(ngram.v, start=2, stop=2)
ngram.p <- split(ngram.v, f=phonem)
ngram.p.t <- lapply(ngram.p, table)
return(ngram.p.t)
}

.make.table <- function(ngram.table) {
  f <- "out_mngram.html";
cat("<html>", file=f, append=FALSE)
names <- names(ngram.table)
for (i in 1:(length(ngram.table))) {
  HTML(paste("<h1>/", names[i], "/</h1>"), file=f);
  t <- ngram.table[[i]];
  m <- as.matrix(t);
  m <- m[order(m[,1], decreasing=T),, drop=F]
  HTML(m, file=f)
}
cat("</html>", file=f, append=TRUE)

}

#c <- readLines('/Users/sylvainloiseau/Recherche/Terrain/Corpus/Transcriptions/Toolbox/Texts/2014.VI.txt')
#sapply(c, strsplit, split="\\W+")

.make.minimal.pair <- function(dictionary) {
  ## paire minimale
names(lex) <- lex
x <- adist(lex, count=T)
cn <- colnames(x)
rn <- rownames(x)
r <- row(x)
c <- col(x)
df <- data.frame(lex1=rn[as.vector(r)],
                 lex2=cn[as.vector(c)],
                 dist=as.vector(x),
                 ins=as.vector((attr(x,"counts"))[,,"ins"]),
                 del=as.vector((attr(x,"counts"))[,,"del"]),
                 sub=as.vector((attr(x,"counts"))[,,"sub"])
)

df[df[,"dist"] == 1 & df[,"sub"] == 1,]
}

