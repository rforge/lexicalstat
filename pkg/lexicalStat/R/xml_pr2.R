#
#
# Test de performance des différentes méthodes XML
# some of the methods have been copied into xml.R
#
#

pr.def <- function() {
  elementcontent2list.sax("pr_def.xml", element="def", attribute=NULL, value=NULL);
}
#def <- pr.def();
#save(def, file="../data/pr_def.rda")

pr.cit <- function() {
  elementcontent2list.sax("pr_cit.xml", element="quote", attribute=NULL, value=NULL);
}
#pr_quote <- pr.cit();
#save(pr_quote, file="../data/pr_quote.rda")

pr.exe.forge <- function() {
  elementcontent2list.sax("pr_exe.xml", element="eg", attribute=NULL, value=NULL);
}
#pr_exe <- pr.exe.forge();
#save(pr_exe, file="../data/pr_exe.rda")

pr.cit.lem <- function() {
  elementcontent2list.sax("pr_cit.xml", element="quote", attribute=NULL, value=NULL);
}

##
## Test de performance de 3 méthodes
##
## (sur panini2, avec le Petit Robert)
##
##> test(filename="../data/pr.xml")
##[1] "sax-------------"
##[1] "Fri Apr 16 11:27:35 2010"
##[1] "found: 167360 eg"
##[1] 167360
##[1] "xpath------------"
##[1] "Fri Apr 16 11:28:17 2010"
##[1] 167360
##[1] "tree with handlers------------"
##[1] "Fri Apr 16 11:30:02 2010"
##[1] "found: 167360 eg"
##[1] 167360
##[1] "Fri Apr 16 11:31:21 2010"
##
## bilan : 42'' pour SAX, 1'41'' pour XPath, 1'19'' pour xmlTreeParse avec handler
##
  ###
  #Première méthode : pur sax
  ###
  #elementcontent2list.sax(filename, element="eg", attribute="type", value="forge");

  ###
  #Seconde méthode : sax avec "branch"
  ###
  #elementcontent2list.sax.branching(filename, element="eg", attribute="type", value="forge");

  ###
  #Troisième : TreeParse. XPath
  ###
  #elementcontent2list.xpath(filename, "//*[local-name()='eg']");

  ###
  #Quatrième : TreeParse, avec handler.
  ###
  #elementcontent2list.tree.handlers(filename, "eg", "type", "forge");

  #  filename = system.file("exampleData", "branch.xml", package="XML")

test <- function(filename="pr_test.xml", element="eg", attribute="type", value="forge") {
  print("-------------");
  print("sax");
  print(date());
  l <- elementcontent2list.sax(filename, element="eg", attribute="type", value="forge");
  print(length(l));
  #print("sax with branching------------");
  #print(date());
  #l <- elementcontent2list.sax.branching(filename, element="eg", attribute="type", value="forge");
  #print(length(l));
  print("------------");
  print("xpath");
  print(date());
  l <- elementcontent2list.xpath(filename, "//*[local-name()='eg' and @type='forge']");
  print(length(l));
  print("------------");
  print("tree with handlers");
  print(date());
  l <- elementcontent2list.tree.handlers(filename, "eg", "type", "forge");
  print(length(l));
  print(date());
}

## TODO : à finir
## Fait un arbre de liste à partir de l'arbre XML. Attention, ignoreBlanks=TRUE
##
elementcontent2tree.tree.handlers <- function(filename, elements, finals) {
  nbr.elements <- length(elements);
  nbr.finals <- length(finals);

  handlers <- function() { 
    l <- list();
    for (i in 1:7) {
      l[[i]] <- function(x){ NULL };
    }
    for (i in 8:(8+nbr.elements)) {
      l[[i]] <- function(x){ return(x) };
    }
    for (i in (8+nbr.elements):(8+nbr.elements+nbr.finals)) {
      l[[i]] <- function(x){ xmlValue(x) };
    }
    names(l) <- c(".startElement", '.comment', '.externalEntity', '.processingInstruction', '.text', '.cdata', '.namespace', elements, finals);
    return(l);
  }
  h <- handlers();

  doc <- xmlTreeParse(filename,  handlers=h, asTree=TRUE, useInternalNodes=FALSE, useDotName=TRUE, trim=FALSE, ignoreBlanks=TRUE, xinclude=FALSE);
  # useInternalNodes ?
  return(.tree2list(doc$doc$children));
}

.tree2list <- function(doc) {
  if (!is.list(doc) & ! "XMLNode" %in% class(doc) ) {
    return(doc);
  }
  nodes <- doc;
  #attributes(nodes) <- NULL;
  class(nodes) <- NULL;
  for (i in length(nodes):1) {
    if (is.list(nodes[[1]])) {
      print(xmlName(nodes[[1]]));
      if (length(nodes[[i]]) == 0) {
        nodes[[i]] <- NULL;
      } else {
        nodes[[i]] <- .tree2list(nodes[[i]]$children);
      }
    }
  }
  return(nodes);
}


elementcontent2list.tree.handlers <- function(filename, element, attribute=NULL, value=NULL) {
  # get the number of node matching
  nbr <- .get.nbr.of.occ(filename, element, attribute, value);
  #INCREMENT <- 1;

  handlers <- function() { 
    #vars <- vector(mode = "list", length=INCREMENT);
    vars <- vector(mode = "list", length=nbr);
    #max <- INCREMENT;
    index <- 1;

    l <- list(
        function(x, attrs) { 
        if (.attribute.condition.dom(x, attribute, value)) {
       # if (index > max) {
       #    vars <<- c(vars, vector(mode = "list", length=INCREMENT));
       #    max <<- max + INCREMENT;
       # }
        vars[index] <<- xmlValue(x[[1]]); 
        index <<- index + 1;
        #print(index);
        NULL
        }
        }
        , 
        function(x){ NULL }, 
        function(x){ NULL }, 
        function(x){ NULL }, 
        function(x){ NULL }, 
        function(x){ NULL }, 
        function(x){ NULL }, 
        function(x){ NULL }, 
        
        function() {
        #return(vars[1:(index-1)])
        return(vars)
        }
        );
        names(l) <- c(element, ".startElement", '.comment', '.externalEntity', '.processingInstruction', '.text', '.cdata', '.namespace',"getVars");
        return(l);
  }
  h <- handlers();

  doc <- xmlTreeParse(filename,  handlers=h, asTree=FALSE, useInternalNodes=TRUE, useDotName=TRUE, trim=FALSE, ignoreBlanks=FALSE, xinclude=FALSE);
  return(h$getVars());
}

elementcontent2list.xpath <- function(filename, xpath) {
  doc = xmlTreeParse(filename, useInternalNodes = TRUE, trim=FALSE, ignoreBlanks=FALSE, xinclude=FALSE);
  els = getNodeSet(doc, xpath, fun=xmlValue);
  # ‘getNodeSet(d, "//d:myNode", c(d = "http://www.omegahat.org"))’ to
  #  match myNode in the default name space ‘http://www.omegahat.org’.
  return(as.character(els));
}

elementcontent2list.sax.branching <- function (filename, element="eg", attribute="type", value="forge") {

## First we compute the number of element found
  nbr <- .get.nbr.of.occ(filename, element, attribute, value);

  clos = function() {
    vals <- character(nbr);
    index <- 1;
    f = function(node) {
      if (.attribute.condition.dom(node, attribute, value)) {
        vals[index] <<- xmlValue(node);
        index <<- index + 1;
      }
     # v <- xmlGetAttr(node, attribute);
     # if (length(v) > 0) {
     #   if (v == value) {
     #   }
     # }
    };
    l <- list(f, function() vals)
    names(l) <- c(element, "getVals");
    return(l);
  }

  b = clos();
  invisible(xmlEventParse(filename, branches = b[element]), ignoreBlanks=FALSE, trim=FALSE);
  b$getVals();
}

elementcontent2list.sax <- function(filename="../pr.xml", element="def", attribute=NULL, value=NULL) {
  if (!is.character(element)) stop("element must be a character vector");
  if (length(element) != 1) stop("element length must be 1");
  if (!is.character(filename)) stop("filename must be a character vector");
  if (length(filename) != 1) stop("filename length must be 1");

  ## first, count the number of element
  n <- .get.nbr.of.occ(filename, element, attribute, value);

  extract.content.closure <- function () {
    content <- character(n);
    index <- 1;
    isInElement <- FALSE;
    current <- "";
    depth <- 0;

    endElement <- function(name, namespace) {
      if (identical(name, element) & depth == 1) { # 1, not 0, in order to skip other matching endElement!
        content[index] <<- current;
        index <<- index + 1;
        isInElement <<- FALSE;
        depth <<- 0;
      }
      if(isInElement) depth <<- depth - 1;
    }

    startElement <- function(name, attrs, namespace, allNamespaces) {
      if (identical(name, element) & depth == 0) {
        match <- .attribute.condition.sax(attrs, attribute, value);
        if (match) {
          isInElement <<- TRUE;
          current <<- "";
        }
      }
      if(isInElement) depth <<- depth + 1;
    }

    text <- function(node) {
      if (isInElement) {
        current <<- paste(current, node, sep="");
      }
    }

    list("startElement"=startElement, "endElement"=endElement, text=text, getContent=function() content );
  }
  h = extract.content.closure();
  invisible(xmlEventParse(filename, handlers = h, saxVersion = 2, useTagName=FALSE, ignoreBlanks=FALSE, trim=FALSE, addContext=FALSE));
  h$getContent();
}

# count using sax the number of times an element with given condition is encoutered.
# the condition are attribute and attribute value.
# if attribute is NULL, no condition is tested on element. If only value is NULL, no
# condition is tested on the attribute of the given name.
.get.nbr.of.occ <- function(filename, element, attribute=NULL, value=NULL) {
 # clos.count = function() {
 #   nbr <- integer(1);
 #   f = function(node) {
 #     if (.attribute.condition.dom(node, attribute, value)) {
 #       nbr <<- nbr + 1;
 #     }
 #    # v <- xmlGetAttr(node, attribute);
 #    # if (length(v) > 0) {
 #    #   if (v == value) {
 #    #   }
 #    # }
 #   };
 #   l <- list(f, function() nbr)
 #   names(l) <- c(element, "getNbr");
 #   return(l);
 # }
 # c.count = clos.count();
 # invisible(xmlEventParse(filename, branches = c.count[element]));
 # nbr <- c.count$getNbr();
 # print(nbr);

  count.elem.closure <- function () {
    nbr <- 0;
    startElement <- function(name, attrs, namespace, allNamespaces) {
      if (identical(name, element)) {
        match <- .attribute.condition.sax(attrs, attribute, value);
        if (match) {
          nbr <<- nbr + 1;
        }
      }
    }
    list(".startElement"=startElement, getNbr=function() nbr);
  }
  nbr = count.elem.closure();
  invisible(xmlEventParse(filename, handlers = nbr, saxVersion = 2, ignoreBlanks=FALSE, useTagName=FALSE, addContext=FALSE, useDotName=TRUE, trim=FALSE));
  nbr <- nbr$getNbr();
  print(paste("found:", nbr, element));
  return(nbr);
}

##
## Check a condition expressed on an attributes with SAX object. See .attribute.condition.dom()
##
.attribute.condition.sax <- function(attrs, attribute, value) {
  if (is.null(attribute)) {
    return(TRUE)
  }
  else {
    if (exists("attrs") & ! is.null(attrs) & length(attrs) > 0) {
      if (any(names(attrs) %in% attribute)) {
        if (is.null(value)) {
          return(TRUE);
        } else if (attrs[attribute] == value) {
          return(TRUE);
        }
      }
    }
  }
  return(FALSE);
}

## the condition to test on each node "element"
## attribute and value may be null : if attribure is null, no attribute is tested; if value is null, the presence
## of attribute is tested, but not its value.
.attribute.condition.dom <- function(node, attribute, value) {
  if (is.null(attribute)) return(TRUE);
  v <- xmlGetAttr(node, attribute);
  if (length(v) == 0) {
    return(FALSE);
  } else {
    if (is.null(value)) {
      return(TRUE);
    } else if (value == v) {
      return(TRUE);
    } else {
      return(FALSE);
    }
  }
}


#elementcontent2list <- function(filename="../pr.xml", element="def") {
#  if (!is.character(element)) stop("element must be a character vector");
#  if (length(element) != 1) stop("element length must be 1");
#  if (!is.character(filename)) stop("filename must be a character vector");
#  if (length(filename) != 1) stop("filename length must be 1");
#
#  count.elem.closure <- function () {
#    nbr <- 0;
#    startElement <- function(name, attrs, namespace, allNamespaces) {
#      if (name == element) {
#        nbr <<- nbr + 1;
#      }
#    }
#    list("startElement"=startElement, getNbr=function() nbr);
#  }
#  nbr = count.elem.closure();
#  invisible(xmlEventParse(filename, handlers = nbr, saxVersion = 2, useTagName=FALSE, ignoreBlanks=FALSE, addContext=FALSE));
#  n <- nbr$getNbr();
#  print(paste(n, element));
#
#  extract.content.closure <- function () {
#    content <- character(n);
#    index <- 1;
#    isInElement <- FALSE;
#    current <- "";
#
#    endElement <- function(name, namespace) {
#      if (identical(name, element)) {
#        content[index] <<- current;
#        index <<- index + 1;
#        isInElement <<- FALSE;
#      }
#    }
#
#    startElement <- function(name, attrs, namespace, allNamespaces) {
#      if (identical(name, element)) {
#        isInElement <<- TRUE;
#        current <<- "";
#      }
#    }
#
#    text <- function(node) {
#      if (isInElement) {
#        current <<- paste(current, node, sep=" ");
#      }
#    }
#
#    list("startElement"=startElement, "endElement"=endElement, text=text, getContent=function() content );
#  }
#  h = extract.content.closure();
#  invisible(xmlEventParse(filename, handlers = h, saxVersion = 2, useTagName=FALSE, ignoreBlanks=FALSE, addContext=FALSE));
#  h$getContent();
#}

# A contingency table through a individu (element name) a variables as an
# element name, and an attribute name.

xml.attr2lexical.table <- function(filename, individu, variable, attr_name) {
  if (is.character(individu)) {
    individu_element <- individu;
    individu <- function(localname) {
      if (localname == individu_element) return(TRUE)
      else return(FALSE)
    }
  } else if (!class(individu) == "function") stop("individu must be a character vector or a function");

  if (is.character(variable)) {
    variable_element <- variable;
    variable <- function(localname) {
      #print(variable_element);
      #print(localname);
      if (localname == variable_element) return(TRUE)
      else return(FALSE);
    }
  } else if (!class(variable) == "function") stop("variable must be a character vector or a function");

  if (!is.character(attr_name)) stop("attr must be a character");

  closure <- function () {
    vars <- character(0);
    lexical.table <- data.frame();
    endElement <- function(name, namespace) {
      if (individu(name)) newobs();
    }
    startElement <- function(name, attrs, namespace, allNamespaces) {
      #print(paste("---", name, "---"));
      if (variable(name)) {
        if (length(attrs) > 0) {
          if (any(names(attrs) %in% attr_name)) {
            var <- attrs[attr_name];
            if (var != "") {
              vars <<- append(vars, var);
            }
          }
        }
      }
    }
    newobs <- function() {
      lexical.table <<- add.to.lexical.table(lexical.table, vars);
      vars <<- character(0);
    }
    list("startElement"=startElement, "endElement"=endElement, getTable=function() lexical.table );
  }
  h = closure();
  invisible(xmlEventParse(filename, handlers = h, saxVersion = 2, useTagName=F, ignoreBlanks=FALSE));
  h$getTable();
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Utilities functions for writing as XML
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.saveListAsXML <- function (l, filename) {
  write("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>", file=filename);
  write("<list>", file=filename, append=T);
  for (i in 1:length(l)) {
    write("<item>", file=filename, append=T);
    write("<label>", file=filename, append=T);
    write(names(l)[i], file=filename, append=T);
    write("</label>", file=filename, append=T);
    write("<data>", file=filename, append=T);
    write(l[[i]], file=filename, append=T);
    write("</data>", file=filename, append=T);
    write("</item>", file=filename, append=T);
  }
  write("</list>", file=filename, append=T);
}

.saveDataFrameAsXML <- function(data, filename) {
  r <- nrow(data);
  c <- ncol(data);
  rname <- rownames(data);
  cname <- colnames(data);
  write("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>", file=filename);
  write("<table>", file=filename, append=T);
  if (length(cname) > 0) {
    write("<row role=\"label\">", file=filename, append=T);
    write("<cell/>", file=filename, append=T);
    for(i in cname) {
      write("<cell>", file=filename, append=T);
      write(i, file=filename, append=T);
      write("</cell>", file=filename, append=T);
    }
    write("</row>", file=filename, append=T);
  }
  if (length(r) > 0) {
    for(i in 1:r) {
      write("<row role=\"data\">", file=filename, append=T);
      write("<cell role=\"label\">", file=filename, append=T);
      write(rname[i], file=filename, append=T);
      write("</cell>", file=filename, append=T);
      for(j in 1:c) {
        write("<cell>", file=filename, append=T);
        write(data[i,j], file=filename, append=T);
        write("</cell>", file=filename, append=T);
      }
      write("</row>", file=filename, append=T);
    }
  }
  write("</table>", file=filename, append=T);
}

