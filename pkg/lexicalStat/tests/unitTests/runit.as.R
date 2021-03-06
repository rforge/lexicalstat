############################################################
##
## As Tabulated
##
############################################################

test_as.Tabulated.fullText <- function() {
  data(dickensFullText);
  i <- dickensFullText;
  d <- as.Tabulated(i);
  equals_corpus_tabulated(d, i); # watch order for "word" argument to be thrown in equals_corpus_tabulated
}

############################################################
##
## As FullText
##
############################################################

test_as.FullText.tabulated <- function() {
  data(dickensTabulated);
  i <- dickensTabulated;
  d <- as.FullText(i, "word", "sentence");
  equals_corpus_tabulated(i, d);
}

############################################################
##
## As LexicalTable
##
############################################################

# TODO : tester si d'autres paramètres que "word" sont pris en compte
test_as.LexicalTable.Tabulated <- function() {
  data(dickensTabulated);
  i <- dickensTabulated;
  d <- as.LexicalTable(i, "word", "sentence");
  equals_corpus_tabulated(i, d);
}

test_as.LexicalTable.Tabulated_frequencies <- function() {
  corpus <- LETTERS[sample(26, 50, replace=TRUE)];
  df <- data.frame(a=as.factor(corpus), b=as.factor(rep(c(1,2), each=25)));
  x <- tabulated(df, 1);

  y <- as.LexicalTable(dickensTabulated, "a", "b");
  z <- as.FrequencyList(y);
  
  n <- table(corpus)
  checkEqualsNumeric(n, freq(z, names(n)));
}

test_as.LexicalTable.FullText <- function() {
  data(dickensFullText);
  i <- dickensFullText;
  d <- as.LexicalTable(i);
  equals_corpus(i, d);
}

test_as.LexicalTable.FullText_frequencies <- function() {
  corpus <- LETTERS[sample(26, 50, replace=TRUE)];
  ft <- fullText(list(corpus=corpus));
  lt <- as.LexicalTable(ft);
  
  x <- table(unlist(ft));
  
  y <- rowSums(lt);
  names(y) <- rownames(lt);
  checkEqualsNumeric(x, y[names(x)]);
}

############################################################
##
## As FrequencyList
##
############################################################

# TODO : tester si d'autres paramètres que "word" sont pris en compte
test_as.FrequencyList.Tabulated <- function() {
  data(dickensTabulated);
  i <- dickensTabulated;
  d <- as.FrequencyList(i, "word");
  equals_corpus_tabulated(i, d);
}

test_as.FrequencyList.FullText <- function() {
  data(dickensFullText);
  i <- dickensFullText;
  d <- as.FrequencyList(i);
  equals_corpus(i, d);
}

test_as.FrequencyList.LexicalTable <- function() {
  data(dickensLexicalTable);
  i <- dickensLexicalTable;
  d <- as.FrequencyList(i);
  equals_corpus(i, d);
}

equals_corpus <- function(c1, c2) {
  c1 <- as.FrequencyList(c1);
  c2 <- as.FrequencyList(c2);
  checkEqualsNumeric(N(c1), N(c2));
  checkEqualsNumeric(ntype(c1), ntype(c2));
  checkEqualsNumeric(types(c1), types(c2));  
}

# may do exactly the same thing as uprise, if "word" was added as default argument
# in ntype and types method signatures
equals_corpus_tabulated <- function(c1, c2) {
  c1 <- as.FrequencyList(c1, "word");
  equals_corpus(c1, c2);
}