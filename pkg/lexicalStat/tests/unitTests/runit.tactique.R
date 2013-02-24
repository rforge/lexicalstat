############################################################
##
##
##  Test tactique function
##
##
############################################################

test_tactique.get.words.by.slices.by.parts <- function() {
  input <- list(
    part1=c("wordA1", "wordA2", "wordA3", "wordA4"),
    part2=c("wordB1", "wordB2", "wordB3", "wordB4")
  );
  expected <- list(
    part1=list("1"=c("wordA1", "wordA2"), "2"=c("wordA3", "wordA4")),
    part2=list("1"=c("wordB1", "wordB2"), "2"=c("wordB3", "wordB4"))
  );
  checkEquals(expected, lexicalStat:::.get.words.by.slices.by.parts(input, 2));
}

test_tactique.flip.inner.outer.parts <- function() {
  input <- list(
    part1=list("1"=c("wordA1", "wordA2"), "2"=c("wordA3", "wordA4")),
    part2=list("1"=c("wordB1", "wordB2"), "2"=c("wordB3", "wordB4"))
  );
  expected <- list(
    list("part1"=c("wordA1", "wordA2"), "part2"=c("wordB1", "wordB2")),
    list("part1"=c("wordA3", "wordA4"), "part2"=c("wordB3", "wordB4"))
  );
  checkEquals(expected, lexicalStat:::.flip.inner.outer.parts(input, 2));
}

test_tactique.remove.inner.part <- function() {
  input <- list(
    list("part1"=c("wordA1", "wordA2"), "part2"=c("wordB1", "wordB2")),
    list("part1"=c("wordA3", "wordA4"), "part2"=c("wordB3", "wordB4"))
  );
  expected <- list(
    c("wordA1", "wordA2", "wordB1", "wordB2"),
    c("wordA3", "wordA4", "wordB3", "wordB4")
  );
  checkEquals(expected, lexicalStat:::.remove.inner.part(input));
}

test_tactique_.get.sub.corpus.in.tactique_ps_s_w <- function() {
  corpus <- .get.corpus.test();
  expected <- list(slice=list(part2=c("trois", "deux")));
  found <- lexicalStat:::.get.sub.corpus.in.tactique(corpus, "trois", 1, "ps(s,w)");
  checkEquals(expected, found);
}

# TODO : other parameters in this function + other functions.

.get.corpus.test <- function() {
 corpus <- list(
 slice1=list(
   part1=c("un", "deux"),
   part2=c("trois", "deux")),
 slice2=list(
   part1=c("trois", "quatre"),
   part2=c("quatre", "cinq")),
 slice3=list(
   part1=c("un", "trois"),
   part2=c("trois", "quatre")
 )
 );
}

test_tactique.get.words.by.slices <- function() {
  input <- list(
    part1=c("wordA1", "wordA2", "wordA3", "wordA4"),
    part2=c("wordB1", "wordB2", "wordB3", "wordB4")
  );
  expected <- list(
    "slice 1"=c("wordA1", "wordA2", "wordB1", "wordB2"),
    "slice 2"=c("wordA3", "wordA4", "wordB3", "wordB4")
  );
  input <- fullText(input);
  expected <- fullText(expected);
  found <- lexicalStat:::.get.words.by.slices(input, 2);
  checkEquals(expected, found);
}

