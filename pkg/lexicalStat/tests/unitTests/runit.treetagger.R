test_read.treetagger_file_with_xml <- function() {
  fileName <- system.file(c("inst", "exempleData"), "PetitLarousse1905.ttg", package="lexicalStat")
  c <- read.treetagger(fileName);
}

test_read.treetagger_file_doesnot_exist <- function() {
  fileName <- system.file(c("inst", "exempleData"), "foobar.ttg", package="lexicalStat")
  checkException(read.treetagger(fileName));
}

test_read.reetagger_test_header <- function() {
  fileName <- system.file(c("inst", "exempleData"), "sample.ttg", package="lexicalStat")
  c <- read.treetagger(fileName);
  checkEquals(c("word", "pos", "lemma", "cit", "def", "EntryFree", "form"), colnames(c));
}

test_read.reetagger_discard.xml <- function() {
  fileName <- system.file(c("inst", "exempleData"), "sample.ttg", package="lexicalStat")
  c <- read.treetagger(fileName, discard.xml=TRUE);
  checkEquals(c("word", "pos", "lemma"), colnames(c));
}

test_read.reetagger_contains.xml.no <- function() {
  fileName <- system.file(c("inst", "exempleData"), "sample_noxml.ttg", package="lexicalStat")
  c <- read.treetagger(fileName, contains.xml=FALSE);
  checkEquals(c("word", "pos", "lemma"), colnames(c));
}

# contains.xml=TRUE but the file does not contain XML.
test_read.reetagger_contains.error.no.xml.found <- function() {
  fileName <- system.file(c("inst", "exempleData"), "sample_noxml.ttg", package="lexicalStat")
  checkException(read.treetagger(fileName, contains.xml=TRUE));
}

