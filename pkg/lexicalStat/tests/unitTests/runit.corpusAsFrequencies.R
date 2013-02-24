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

corpus_common <- function(i) {
  checkTrue(is(i, "CorpusAsFrequencies"));
  checkEqualsNumeric(34, N(i));
  checkEqualsNumeric(26, ntype(i));
  checkEqualsNumeric(26, length(types(i)));
}
