############################################################
##
## is.subcorpus.of
##
############################################################

tests_is.subcorpus_TRUE_equals <- function() {
 fl <- frequencyList(c(type1=10, type2=20, type3=30));
 checkTrue(is.subcorpus.of(fl, fl));
}

tests_is.subcorpus_TRUE_subfreq <- function() {
 fl <- frequencyList(c(type1=10, type2=20, type3=30));
 sub.fl <- frequencyList(c(type1=1, type2=20, type3=9));
 checkTrue(is.subcorpus.of(sub.fl, fl));
}

tests_is.subcorpus_TRUE_reordered <- function() {
 fl <- frequencyList(c(type1=10, type2=20, type3=30));
 sub.fl <- frequencyList(c(type2=20, type1=1, type3=9));
 checkTrue(is.subcorpus.of(sub.fl, fl));
}

tests_is.subcorpus_TRUE_type.missing <- function() {
 fl <- frequencyList(c(type1=10, type2=20, type3=30));
 sub.fl <- frequencyList(c(type1=1, type2=20));
 checkTrue(is.subcorpus.of(sub.fl, fl));
}

tests_is.subcorpus_FALSE_freq <- function() {
 fl <- frequencyList(c(type1=10, type2=20, type3=30));
 sub.fl <- frequencyList(c(type1=11, type2=20, type3=30));
 checkTrue(! is.subcorpus.of(sub.fl, fl));
}

tests_is.subcorpus_FALSE_types <- function() {
 fl <- frequencyList(c(type1=10, type2=20, type3=30));
 sub.fl <- frequencyList(c(type1=10, type2=20, type3=30, type4=3));
 checkTrue(! is.subcorpus.of(sub.fl, fl));
}