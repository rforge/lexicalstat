test_corpus_isCorpus_frequencyList <- function () {
  data(dickensFrequencyList);
  i <- dickensFrequencyList;
  corpus_common(i);
}

test_corpus_isCorpus_LexicalTable <- function () {
  data(dickensLexicalTable);
  i <- dickensLexicalTable;
  corpus_common(i);
}

test_corpus_isCorpus_FullText <- function () {
  data(dickensFullText);
  i <- dickensFullText;
  corpus_common(i);
}

test_corpus_isCorpus_Tabulated <- function () {
  data(dickensTabulated);
  i <- dickensTabulated;
  corpus_common_tabulated(i);
}

corpus_common <- function(i) {
  checkTrue(is(i, "Corpus"));
  checkEqualsNumeric(34, N(i));
}

corpus_common_tabulated <- function(i) {
  checkTrue(is(i, "Corpus"));
  checkEqualsNumeric(34, N(i));
}