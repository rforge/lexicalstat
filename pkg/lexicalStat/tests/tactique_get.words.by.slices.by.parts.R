library(textcorpus);

##
## Test 1
##

l <- list("un possible ennui.", "un vrai emmerdement");
tokenized <- tokenize(as.character(l));

found <- get.words.by.slices.by.parts(tokenized, nslice=3, min=3);
print(found);

expected <- list(
    list(
      "1"="un",
      "2"="possible",
      "3"="ennui"
      ),
    list(
      "1"="un",
      "2"="vrai",
      "3"="emmerdement"
      )
    );

identical(found, expected);

##
## Test 1 : applying "min" argument
##

l <- list("un possible ennui un vrai emmerdement.", "un vrai emmerdement");
tokenized <- tokenize(as.character(l));

found <- get.words.by.slices.by.parts(tokenized, nslice=3, min=6);
print(found);

expected <- list(
    list(
      "1"=c("un", "possible"),
      "2"=c("ennui", "un"),
      "3"=c("vrai", "emmerdement")
      )
    );

identical(found, expected);

