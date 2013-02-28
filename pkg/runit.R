library(RUnit);
library(lexicalStat);
testSuite <- defineTestSuite(name=paste(pkg, "unit testing"), dirs="unitTests");
tests <- runTestSuite(testSuite);
