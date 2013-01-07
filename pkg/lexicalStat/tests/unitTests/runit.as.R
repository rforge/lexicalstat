############################################################
##
## As Tabulated
##
############################################################

test_asTabulated.fullText <- function() {
  data(dickensFullText);
  i <- dickensFullText;
  d <- asTabulated(i);
  equals_corpus_tabulated(d, i); # watch order for "word" argument to be thrown in equals_corpus_tabulated
}

############################################################
##
## As FullText
##
############################################################

test_asFullText.tabulated <- function() {
  data(dickensTabulated);
  i <- dickensTabulated;
  d <- asFullText(i, "word", "sentence");
  equals_corpus_tabulated(i, d);
}

############################################################
##
## As LexicalTable
##
############################################################

# TODO : tester si d'autres paramètres que "word" sont pris en compte
test_asLexicalTable.Tabulated <- function() {
  data(dickensTabulated);
  i <- dickensTabulated;
  d <- asLexicalTable(i, "word", "sentence");
  equals_corpus_tabulated(i, d);
}

test_asLexicalTable.FullText <- function() {
  data(dickensFullText);
  i <- dickensFullText;
  d <- asLexicalTable(i);
  equals_corpus(i, d);
}

############################################################
##
## As FrequencyList
##
############################################################

# TODO : tester si d'autres paramètres que "word" sont pris en compte
test_asFrequencyList.Tabulated <- function() {
  data(dickensTabulated);
  i <- dickensTabulated;
  d <- asFrequencyList(i, "word");
  equals_corpus_tabulated(i, d);
}

test_asFrequencyList.FullText <- function() {
  data(dickensFullText);
  i <- dickensFullText;
  d <- asFrequencyList(i);
  equals_corpus(i, d);
}

test_asFrequencyList.LexicalTable <- function() {
  data(dickensLexicalTable);
  i <- dickensLexicalTable;
  d <- asFrequencyList(i);
  equals_corpus(i, d);
}

equals_corpus <- function(c1, c2) {
  checkEqualsNumeric(N(c1), N(c2));
  checkEqualsNumeric(V(c1), V(c2));
  checkEqualsNumeric(types(c1), types(c2));  
}

# may do exactly the same thing as uprise, if "word" was added as default argument
# in V and types method signatures
equals_corpus_tabulated <- function(c1, c2) {
  checkEqualsNumeric(N(c1), N(c2));
  checkEqualsNumeric(V(c1, "word"), V(c2));
  checkEqualsNumeric(types(c1, "word"), types(c2));  
}