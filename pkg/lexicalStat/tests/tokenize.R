library(textcorpus);
l <- list(c("un possible ennui."), "un vrai emmerdement");
tokenize(as.character(l));

l <- list(c("Caractères accentués"));
tokenize(as.character(l));
