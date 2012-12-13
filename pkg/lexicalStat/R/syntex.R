#fileName <- system.file("exampleData", "test.xml", package="textcorpus")
#fileName <- "/home/sloiseau/workspace/dicograph/src/main/R/dicograph/exempleData/syntex_verylong.xml"
fileName <- "/Users/sylvainloiseau/workspace/textcorpus/exempleData/syntex_tiny_ou.xml"

#filter.seq= function(seq) {print("---"); print(seq); if (! "entre" %in% seq$tokens) return(NULL)};

## apply a function on each DOM node "SEQ" in a syntex file
## the DOM node is constructed only for SEQ, so that huge file may be processed.
seq.apply <- function(filename, fun) {
  if (mode(fun) != "function") stop("fun must be a function");
  seq_callback = function(node) {
    fun(node);
  };
  invisible(xmlEventParse(filename, ignoreBlanks=T, branches = list("SEQ"=seq_callback)));
}

## apply a function on each SEQ node and collect the values returned in a list.
seq.collect <- function(filename=fileName, fun) {
  if (mode(fun) != "function") stop("fun must be a function");
  collect.seqs = function() {
    seqs <- list();
    f = function(node) {
     x <- fun(node);
     seqs <<- append(seqs, x);
    };
    list(SEQ = f, get.seqs = function() seqs);
  }

  b = collect.seqs();
  invisible(xmlEventParse(filename, ignoreBlanks=T, branches = b["SEQ"]));
  b$get.seqs();
}

##
## Convert a SEQ element in a R list where:
## list(
##   id = character(), # the syntex id attribute of the SEQ element
##   tokens=character(), # the lemma, index of element in vector give id.
##   source=numeric(),
##   target=numeric(),
##   relation=character() # the type of the relation
## );
## only the "d" dependancies are used.
## Return NULL if there is not a text, a list of tokens and a list of dependancies in the node.
## Warning: node SEQ must be constructed with ignoreBlanks=T.
##
seq.node2list <- function(node) {
  id <- xmlGetAttr(node, "id");
  if (length(xmlChildren(node)) != 3) {
    return(NULL);
  }
  text <- xmlValue(xmlChildren(node)[[1]]);
  tokens <- sapply(xmlChildren(xmlChildren(node)[[2]]), xmlGetAttr, "l");
  dependance <- xmlElementsByTagName(xmlChildren(node)[[3]], "d");
  source <- sapply(dependance, xmlGetAttr, "s");
  target <- sapply(dependance, xmlGetAttr, "c");
  relation <- sapply(dependance, xmlGetAttr, "r");
  myseq <- list(id=id, tokens=tokens, text=text, source=source, target=target, relation=relation);
}

# create an igraph graph representing a SEQ
.get.graph.dependancies <- function(tokens, source, target, relation) {
  if (length(source) != length(target)) stop("source and target length mismatch");
  if (length(source) != length(relation)) stop("source and relation length mismatch");
  if (!is.character(tokens)) stop("tokens must be a character vector");
  if (!is.character(source)) stop("source must be a character vector");
  if (!is.character(target)) stop("target must be a character vector");
  if (!is.character(relation)) stop("relation must be a character vector");

  nbr.relation <- length(source);

  edges <- numeric(nbr.relation*2);
  edges[seq(from=1, length.out=nbr.relation, by=2)] <- as.numeric(source) - 1;
  edges[seq(from=2, length.out=nbr.relation, by=2)] <- as.numeric(target) - 1;
  g <- graph(edges, n=length(tokens));
  V(g)$name = tokens;
  E(g)$name = relation;
  return(g);
}

#################################################

get.seqs.as.list <- function(filename=fileName, filter=NULL) {

  extract.seqs = function() {
    seqs <- list();
    f = function(node) {
      mot <- "tel"; # "entre"
     myseq <- seq.node2list(node);
#      if (!is.null(filter)) {
#        myseq <- filter(myseq);
#        #print(myseq);
#      }
# temporary using:

      if (!is.null(myseq)) {
        seqs[[myseq$id]] <<- myseq;
      }
      g <- .get.graph.dependancies(myseq$tokens, myseq$source, myseq$target, myseq$relation);
      if (! mot %in% V(g)$name) {
        print(paste("skipping", myseq$id, "(", myseq$text, ")"));
        return();
      }
      vertex_ids <- subcomponent(g, V(g)[V(g)$name == mot], mode="all");
      g <- subgraph(g, vertex_ids);

      pdf(paste("~/Bureau/telque/syntextree/", myseq$id, ".pdf", sep=""));
      plot(g, vertex.label=V(g)$name, edge.label=E(g)$name, vertex.size=0, layout=layout.reingold.tilford(g)); #, root=root));
      mtext(.lines(myseq$text, 100), side=1, cex=0.8)
      dev.off();

    };
    list(SEQ = f, get.seqs = function() seqs);
  }

  b = extract.seqs();
  invisible(xmlEventParse(filename, ignoreBlanks=T, branches = b["SEQ"]));
  #b$get.seqs();
}


plot.sentence <- function(fileName) {

  startElement = function(name, attr, .state) {
    if(name == "SEQ") {
    .state$SEQ_name <- attr["id"];
    } else if(name == "TXT") {
    } else if(name == "t") {
      index <- attr["i"];
      lemma <- attr["l"];
      .state$tokens[index] <- lemma;
    } else if (name %in% c("d")) {
      .state$nbr.relation <- .state$nbr.relation + 1;
      .state$source[.state$nbr.relation] <- attr["s"];
      .state$target[.state$nbr.relation] <- attr["c"];
      .state$relation[.state$nbr.relation] <- attr["r"];
    }
    return(.state);
  }

  endElement <- function(name, .state) {
    if(name == "SEQ") {
      #if (length(.state$source) > 0 && length(.state$tokens) > 0 && any(c("entre", "parmi") %in% .state$tokens)) {
      if (length(.state$source) > 0 && length(.state$tokens) > 0) {
        g <- .get.graph.dependancies(.state$tokens, .state$source, .state$target, .state$relation);
        sentence_file <- paste(.state$SEQ_name[1], ".pdf", sep="");

#        t <- .state;
#        root <- which(1:t$nbr.relation %in% t$target & !(1:t$nbr.relation %in% t$source));
#        #print(t$tokens[root]);
#        if (length(root) != 1) {
          root <- 0;
#        } else {
#          root <- root - 1;
#        }
#
print(g);
        pdf(sentence_file);
        plot(g, vertex.label=V(g)$name, edge.label=E(g)$name, vertex.size=0, layout=layout.reingold.tilford(g)); #, root=root));
        dev.off();
      }
   #   .state <- .syntex.initial.state();
    }
    return(.state);
  }

  invisible(xmlEventParse(fileName,  handlers = list(startElement=startElement, endElement=endElement), state=.syntex.initial.state())); # useTagName=T);
}


.syntex.initial.state <- function() {
  return(list(
        tokens = character(0),
        source = character(0),
        target = character(0),
        relation = character(0),
        SEQ_name = character(0),
        text = character(0),
        nbr.relation = 0
        ));
}

# split a string in lines of the given length.

.lines <- function(x, ll=100) {
  if(!is.character(x)) stop("x must be a character vector");
  if(length(x) != 1) stop("x must be of length 1");
  lines <- character(1);
  l <- nchar(x);
  for (i in seq(from=1, to=l, by=ll)) {
    lines <- paste(lines, substr(x, i, i+ll-1), sep="\n");
  }
  return(lines);
}
