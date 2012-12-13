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

# Collect the value of a given attribute on a given element

xml.attr2vector <- function(filename, variable, attr_name) {
  if (is.character(variable)) {
    variable_element <- variable;
    variable <- function(localname) {
      if (localname == variable_element) return(TRUE)
      else return(FALSE);
    }
  } else if (!class(variable) == "function") stop("variable must be a character vector or a function");

  if (!is.character(attr_name)) stop("attr must be a character");

  closure <- function () {
    vars <- character(0);

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
    list("startElement"=startElement, getList=function() vars);
  }
  h = closure();
  invisible(xmlEventParse(filename, handlers = h, saxVersion = 2, useTagName=F, ignoreBlanks=T));
  h$getList();
}


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
    lexical.table <- matrix(numeric(0));
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
      lexical.table <<- .add.to.lexical.table(lexical.table, vars);
      vars <<- character(0);
    }
    list("startElement"=startElement, "endElement"=endElement, getTable=function() lexical.table);
  }
  h = closure();
  invisible(xmlEventParse(filename, handlers = h, saxVersion = 2, useTagName=F, ignoreBlanks=T));
  h$getTable();
}

#######
# extrait la liste des DocId d'un corpus
#######

element.content2list <- function(filename, element) {
  if (!is.character(element)) stop("element must be a character vector");
  if (length(element) != 1) stop("element length must be 1");

  extract.element.content.closure <- function() {
    documents <- list();
    current <- "";
    is_in_element <- F;

    startElement <- function(node, attr) {
      if (node == "DocId") is_in_element <<- TRUE
    }

    endElement <- function(node) {
      if (node == element) {
        documents <<- append(documents, current);
        current <<- "";
        is_in_element <<- F;
      }
    }

    text <- function(node) {
      if (is_in_element) {
        current <<- paste(current, node, sep="");
      }
    }

    return(list("startElement"=startElement, "endElement"=endElement, "text"=text, get.content=function() documents ));
  }

  f <- extract.element.content.closure();
  #invisible(xmlEventParse(filename, ignoreBlanks=T, useTagName=F, handlers = list(f["startElement"], f["endElement"], f["text"])));
  invisible(xmlEventParse(filename, ignoreBlanks=T, useTagName=F, handlers = f));
  return(f$get.content());
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

