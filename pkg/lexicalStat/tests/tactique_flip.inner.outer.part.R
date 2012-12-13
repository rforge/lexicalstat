library(textcorpus);

data(tactique);

expected <- list(
  list(
    partA1=c("word1", "word2"),
    partA2=c("word1", "word2"),
    partA3=c("word1", "word2")
  ),
  list(
    partA1=c("word1", "word2"),
    partA2=c("word1", "word2"),
    partA3=c("word1", "word2")
  )
);

found <- flip.inner.outer.parts(words.by.slices.by.parts.test2, 2);

print(found);
print(expected);

identical(found, expected);

