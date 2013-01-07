# fait à partir de 
# LeMondeEco.lines
# : éclaté sur ".
# :%s/\([^M]\.\+\)/\1\r/g

library(lexicalStat);
sentences <- lines2fullText("LeMondeEcoSentences.txt");
save(sentences, file="sentences.rda");
