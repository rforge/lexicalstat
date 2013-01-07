library(lexicalStat);
source <- "/Users/sylvainloiseau/corpus/corpus Zola/Les Rougon-Macquart/";
names <- c("La Terre.txt", "Nana.txt", "Le Ventre de Paris.txt");
for (n in names) {
   f <- paste(source, n, sep="/")
   x <- readLines(f);
   y <- line2paragraph(x);
   z <- sample(y, 100);
   writeLines(z, paste("zola", n, sep="/"))
}
