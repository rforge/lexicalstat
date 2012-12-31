# Virtual class Corpus
# And methods common to all Corpus sub-classes.

setClass("Corpus");

setIs("FrequencyList", "Corpus");
setIs("LexicalTable", "Corpus");
setIs("FullText", "Corpus");
setIs("Tabulated", "Corpus");

# nforms

# ntokens

# forms

# nhapax

# hapax