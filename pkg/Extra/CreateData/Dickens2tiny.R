library(lexicalStat)
x <- read.table("Dickens.tab", header=FALSE);
colnames(x) <- c("word", "pos", "lemma", "sentence", "np");
x$sentence <- as.numeric(as.factor(x$sentence));
x$np <- as.numeric(as.factor(x$np));

x$sentence <- as.factor(x$sentence);
x$np <- as.factor(x$np);
dickensTabulated <- tabulated(as.data.frame(x), c("word", "pos", "lemma"), c("sentence", "np"));
save(dickensTabulated, file="dickensTabulated.rda");

t <- sapply(split(x$word, x$sentence), as.character)
dickensFullText <- fullText(t);
save(dickensFullText, file="dickensFullText.rda");

dickensFrequencyList <- frequencyList(table(x$word));
save(dickensFrequencyList, file="dickensFrequencyList.rda");

dickensLexicalTable <- lexicalTable(as.matrix(table(x$word, x$sentence)));
save(dickensLexicalTable, file="dickensLexicalTable.rda");


