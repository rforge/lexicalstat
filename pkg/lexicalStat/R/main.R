#
# CMD line functions
#

##
## Compare two text files
##
compare.txts <- function(files, encoding="UTF-8") {
    # corpus is a list (each element (a file) is a vector (of lines))
    cat("Lecture des fichiers\n");
    corpus <- read.corpus.txts(files, enc=encoding, skipEmpty=TRUE);
	# corpus is a is a vector (of files))
    # cat("Constitution des sous-corpus\n");
    # corpus <- sapply(corpus, paste, collapse=" ")
    cat("Tokenization\n");
    tokens <- lapply(corpus, function(x) unlist(tokenize(x)));
    #tokens <- tokenize(corpus)
    cat("Constitution du tableau lexical\n");
    lt <- list2lexical.table(tokens)
    cat("Calcul des spécificités\n");
    specificities(lt);
}

