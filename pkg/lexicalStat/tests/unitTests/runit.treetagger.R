test_read_treetagger_file_with_xml <- function() {
  fileName <- system.file("exempleData", "PetitLarousse1905.ttg", package="lexicalStat")
  c <- read.treetagger(fileName);
}
