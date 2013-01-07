

##
 #
 #
 #
 #
 ##
test_specificities <- function() {

   # Data according to Lafon "Sur la variabilité de la fréquence des formes dans un corpus"
   # Mots, 1980, 1. pp. 127-165.
   # ("peuple" in part "D4").
   # P. 130 (first line)
   # P. 149 (first line)

   x <- specificities(61449,6903,296,14);

   checkEqualsNumeric(x, -3.8733);
}


