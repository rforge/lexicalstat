############################################################
##
## wam.* function
##
############################################################

test_wam.fullText <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "LeMondeEco.small.lines", package="lexicalStat")
  c <- readTexts(fileName);
  wam(c);
}

# not a good idea
#test_wam.tabulated <- function() {
#  fileName <- system.file(c("inst", "exempleData"), "PetitLarousse1905.ttg", package="lexicalStat")
#  c <- read.treetagger(fileName);
#  wam(c, positional="word", structural="def");
#}

test_wam.frequencyList <- function() {
  fileName <- system.file(c("inst", "exempleData"), "PetitLarousse1905.ttg", package="lexicalStat")
  c <- read.treetagger(fileName);
  fl <- as.FrequencyList(c, "word");
  subc <- subcorpus(c, "cit");
  subfl <- as.FrequencyList(subc, "word");
  wam(corpus=fl, subcorpus=subfl);
}

test_wam.lexicalTable <- function() {
   data(dickensLexicalTable)
   wam(dickensLexicalTable);
}
