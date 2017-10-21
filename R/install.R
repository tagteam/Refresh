##' Install an R package
##'
##' This is a wrapper for install.packages which allows the user to forget the quotes.
##' Also, the package is loaded after installation.
##' @title Install an R package
##' @aliases rforge install
##' @param pkg Quoted or unquoted name of the package
##' @param ... passed to install.packages
##' @return see install.packages 
##' @seealso install.packages
##' @examples
##' install(prodlim)
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
install <- function(pkg,...){
    install.packages(as.character(substitute(pkg)),...)
    require(as.character(substitute(pkg)), character.only = TRUE)
}
##' @export 
Install <- install
