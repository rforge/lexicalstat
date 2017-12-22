library(lexicalStat);
robespierre <- read.table("robespierre.tab")
rownames(robespierre) <- tolower(rownames(robespierre));
robespierre <- lexicalTable(robespierre);
save(robespierre, file="robespierre.rda");
