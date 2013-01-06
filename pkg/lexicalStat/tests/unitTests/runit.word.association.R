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
  fl <- asFrequencyList(c, "word");
  subc <- subcorpus(c, "def");
  subfl <- asFrequencyList(subc, "word");
  wam(corpus=fl, subcorpus=subfl);
}

test_wam.lexicalTable <- function() {
   data(robespierre)
   lt <- lexicalTable(robespierre);
   wam(lt);
}

############################################################
##
## wam.numeric function
##
############################################################

test_wam.numeric <- function() {
   wam.num(10, 5, 3, 3);
}

############################################################
##
## Test attraction measure
##
############################################################

test.specificities <- function() {

}

