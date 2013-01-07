test_lines2paragraphs <- function() {
  fileName <- system.file(c("inst",  "exempleData"), "small.paragraphs", package="lexicalStat")
  c <- readTexts(fileName, split.on="paragraphs");
  checkEquals(3, length(c));
}