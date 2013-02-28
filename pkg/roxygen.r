# roxygen2

library(roxygen3)
roxygenise("lexicalStatRoxygen3", check = FALSE, clean = FALSE)
 
library(roxygen2)
roxygenize("lexicalStat", roxygen.dir = "lexicalStatRoxygen", copy.package = FALSE, overwrite = TRUE, unlink.target = FALSE, roclets = c("collate", "namespace", "rd"))
 

# roxygen


library(roxygen)
roxygenize('lexicalStat', roxygen.dir='lexicalStatRoxygen', copy.package=TRUE, unlink.target=FALSE, use.Rd2=TRUE)


##############################################################
#' The Corpus class
#'
#' This virtual class is the root hierarchy of all classes available for representing
#' linguistic corpora.
#'
#' Subclass of \code{Corpus} must implement the generic methods \code{\link{N}}.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{Matrix of class \code{"numeric"}, containing data from slot1}
#'    \item{\code{slot2}:}{Object of class \code{"character"}, containing data that needs to go in slot2.}
#'  }
#'
#' @note You can still add notes
#' @name EXAMPLE 
#' @rdname EXAMPLE
#' @aliases EXAMPLE-class
#' @exportClass EXAMPLE
#' @author Joris Meys

##############################################################
#' The title, in this case: Helloworld-ify the argument.
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @param x Description of \code{x}. The main argument in this
#'  example. Most often has such and such properties.
#'
#' @param y Description of \code{y}. An argument that is rarely
#'  used by \code{"helloworld"} methods. 
#'
#' @param ... Additional argument list that might not ever
#'  be used.
#'
#' @return A helloworld-ified argument. Oh, you'll see.
#' 
#' @seealso \code{\link{print}} and \code{\link{cat}}
#' 
#' @export
#' @docType methods
#' @rdname helloworld-methods
#'
#' @examples
#' helloworld("thisismystring")
#' helloworld(char2helloworld("thisismystring"))
#' helloworld(matrix(0,3,3))
#' helloworld(list(0,0,0))
#' helloworld(integer(0))





##############################################################
#' @rdname helloworld-methods
#' @aliases helloworld,character,character-method
setMethod("helloworld", c("character", "character"), function(x, y){
    show(x)
})