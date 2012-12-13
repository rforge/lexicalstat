library(textcorpus);

##
## Test 1
##

parts <- c("un possible ennui un vrai emmerdement.", "un vrai emmerdement un possible ennui");
words.by.parts <- tokenize(parts);

found <- get.words.by.slices(words.by.parts, 3);
print(found);

expected <- list(
 "slice 1"=c("un", "possible", "un", "vrai"),
 "slice 2"=c("ennui", "un", "emmerdement", "un"),
 "slice 3"=c("vrai", "emmerdement", "possible", "ennui")
)

identical(found, expected);
