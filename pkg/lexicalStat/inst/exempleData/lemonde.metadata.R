lemonde.metadata <- read.table("lemonde.metadata.table", header=TRUE);
lemonde.metadata <- lemonde.metadata [, c("year", "section", "genre", "years.group")];
save(lemonde.metadata, file="lemonde.metadata.rda");
