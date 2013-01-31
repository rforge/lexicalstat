############################################################
##
##
##  Test tactique function
##
##
############################################################

test_tactique_normal_case <- function() {
  input <- list(
    part1=c("wordA1", "wordA2", "wordA3", "wordA4"),
    part2=c("wordB1", "wordB2", "wordB3", "wordB4")
  );
  expected <- list(
    part1=c("wordA1", "wordA2", "wordB1", "wordB2"),
    part2=c("wordA3", "wordA4", "wordB3", "wordB4")
  );
  x <- fullText(input);
  found <- lexicalStat:::.get.words.by.slices(x, 2);
  checkEquals(expected, found);
}

test_tactique_.get.sub.corpus.in.tactique_ps_s_w <- function() {
  c <- .get.corpus.test();
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