setClass("FullText", contains="CorpusAsTokens");

############################################################
##
## Constructor
##
############################################################

fullText <- function(l, depth=1) {
  if(is.null(names(l))) {
    names(l) <- 1:length(l);
  }
  obj <- new("FullTextList", l, depth=depth);
  return(obj);
}

