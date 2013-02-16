test_word.associations_sumary <- function() {
	data(dickensLexicalTable);
	w <- wam(dickensLexicalTable);
	summary(w);
}

