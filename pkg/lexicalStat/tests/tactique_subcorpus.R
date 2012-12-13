library(textcorpus);
data(tactique);

# Rappel :
# words.by.parts.by.slices.test.2 <- list(
#   slice1=list(part1=c("A", "B"), part2=c("a", "b"), part3=c("un", "deux")),
#   slice2=list(part1=c("C", "D"), part2=c("c", "d"), part3=c("trois", "quatre")),
#   slice3=list(part1=c("E", "F"), part2=c("e", "f"), part3=c("cinq", "six")),
#   slice4=list(part1=c("un", "H"), part2=c("i", "j"), part3=c("sept", "A")),
#   slice5=list(part1=c("I", "J"), part2=c("k", "l"), part3=c("neuf", "dix"))
# );

##
## Test 1 : the whole corpus is kept
##

found <- get.sub.corpus.in.tactique(words.by.parts.by.slices.test.2, "un", 1, "c");
expected <- words.by.parts.by.slices.test.2;
identical(found, expected);

##
## Test 2 : the whole part containing the given word at the given slice
## give only part 3, the only part with "un" in slice 1
##

found <- get.sub.corpus.in.tactique(words.by.parts.by.slices.test.2, "un", 1, "p(s,w)");
expected <- list(
  slice1=list(part3=c("un", "deux")),
  slice2=list(part3=c("trois", "quatre")),
  slice3=list(part3=c("cinq", "six")),
  slice4=list(part3=c("sept", "A")),
  slice5=list(part3=c("neuf", "dix"))
);
identical(found, expected);

##
## Test 3 : the whole part containing the given word at any position
## give part 1 and part 3
##

found <- get.sub.corpus.in.tactique(words.by.parts.by.slices.test.2, "A", 1, "p(w)");
expected <- list(
  slice1=list(part1=c("A", "B"), part3=c("un", "deux")),
  slice2=list(part1=c("C", "D"), part3=c("trois", "quatre")),
  slice3=list(part1=c("E", "F"), part3=c("cinq", "six")),
  slice4=list(part1=c("un", "H"), part3=c("sept", "A")),
  slice5=list(part1=c("I", "J"), part3=c("neuf", "dix"))
);
identical(found, expected);

##
## Test 4 : the position containing the given word
## only slice 1-part 1
##
found <- get.sub.corpus.in.tactique(words.by.parts.by.slices.test.2, "A", 1, "ps(s,w)");
#expected <- list(
#  slice=list(part1=c("A", "B"), part2=NULL, part3=NULL)
#);
expected <- list(
  slice=list(part1=c("A", "B"))
);
identical(found, expected);

##
## Test 5 : the position at any position containing the given word
## slice 1-part 1 and slice 4-part 3
##
found <- get.sub.corpus.in.tactique(words.by.parts.by.slices.test.2, "A", 1, "ps(w)");
#expected <- list(
#  slice1=list(part1=c("A", "B"), part2=NULL, part3=NULL),
#  slice2=list(part1=NULL, part2=NULL, part3=NULL),
#  slice3=list(part1=NULL, part2=NULL, part3=NULL),
#  slice4=list(part1=NULL, part2=NULL, part3=c("sept", "A")),
#  slice5=list(part1=NULL, part2=NULL, part3=NULL)
#);
expected <- list(
  slice1=list(part1=c("A", "B")),
  slice4=list(part3=c("sept", "A"))
);
identical(found, expected);


