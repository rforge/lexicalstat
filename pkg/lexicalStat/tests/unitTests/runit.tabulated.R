test_tabulated_subcorpus_structural <- function() {
  fileName <- system.file(c("inst", "exempleData"), "sample.ttg", package="lexicalStat")
  c <- read.treetagger(fileName);
  d <- subcorpus(c, "cit");
  checkEquals(11, nrow(d));
}

test_tabulated_subcorpus_structural_not_found <- function() {
  fileName <- system.file(c("inst", "exempleData"), "sample.ttg", package="lexicalStat")
  c <- read.treetagger(fileName);
  checkException(subcorpus(c, "foo"));
}

test_tabulated_subcorpus_all.arguments <- function() {
  fileName <- system.file(c("inst", "exempleData"), "sample.ttg", package="lexicalStat")
  c <- read.treetagger(fileName);
  d <- subcorpus(c, "cit", "word", "taille");
  checkEquals(11, nrow(d));
}

test_tabulated_subcorpus_all.arguments_positional_not_found <- function() {
  fileName <- system.file(c("inst", "exempleData"), "sample.ttg", package="lexicalStat")
  c <- read.treetagger(fileName);
  checkException(subcorpus(c, "cit", "foobar", "taille"));
}
