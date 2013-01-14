test_word.associations_wordAssociation_implicit.method.name <- function() {
	wordAssociation(10, 5, 7, 4, types="foo", parts="mycorpus");
}

test_word.associations_wordAssociation_explicit.method.name <- function() {
	wordAssociation(10, 5, 7, 4, "wam.specificities", types="foo", parts="mycorpus");
}

test_word.associations_wordAssociation_wrong.method.name <- function() {
	checkException(wordAssociation(10, 5, 7, 4, "wrong_name", types="foo", parts="mycorpus"));
}

