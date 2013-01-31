
test_ngram <- function() {
    x <- structure(list(`1` = c("There is no", "is no doubt", "no doubt whatever",
"doubt whatever about", "whatever about that", "about that ."
), `2` = c("The register of", "register of his", "of his burial",
"his burial was", "burial was signed", "was signed by", "signed by the",
"by the clergyman", "the clergyman ,", "clergyman , the", ", the clerk",
"the clerk ,", "clerk , the", ", the undertaker", "the undertaker ,",
"undertaker , and", ", and the", "and the chief", "the chief mourner",
"chief mourner ."), `3` = c("Scrooge signed it", "signed it ."
)), .Names = c("1", "2", "3"))

  z <- new("FullText", x, depth=1)

  data(dickensFullText);
  y <- ngram(dickensFullText)

  checkEqualsNumeric(class(z), class(y));
  checkEqualsNumeric(length(z), length(y));
  for (i in 1:length(z)) {
    checkEquals(z[[i]], y[[i]]);
  }
}