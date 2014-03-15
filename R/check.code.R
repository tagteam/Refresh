##' Source all R files of a package 
##'
##' This is useful to try changes of a package and to identify syntax failures.
##' @aliases Source
##' @title Source all R files of a package
##' @param pkg Quoted or unquoted name of the package 
##' @param Source place where to look for package
##' @param pattern Matched against file-names in package.
##' @return nothing
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
Source <- check.code
check.code <- function(pkg,
                       Source=options()$refreshSource,
                       pattern=".r$|.R$"){
    pkg <- as.character(substitute(pkg))  
    # {{{  search for an uncompressed package directory with the source code
    Source <- Source[!duplicated(Source)]
    found <- sapply(Source,function(s){file.exists(file.path(s,pkg))})
    if (!any(found))
        stop("No directory ", pkg, " found")
    ## print(file.path(Source,pkg))
    if (sum(found)>1){
        warning("Package source found in two different places.")
        Spath <- select.list(Source[found],multiple=FALSE,title="Select source directory: ")
        SourceDir <- file.path(Spath,pkg)
    }
    else{
        if (any(found)) 
            SourceDir <- file.path(Source[found],pkg)
        else
            SourceDir <- NA
    }
    # }}}
    # {{{ source the R files
    Rlib <- path.expand(paste(SourceDir,"/R",sep=""))
    if (!file.exists(Rlib))
        stop(paste("File",Rlib," does not exist."))
    Rfiles <- list.files(path=file.path(Rlib),pattern=pattern)
    lapply(Rfiles,function(name){
        message(name)
        warning(name)
        source(file.path(Rlib,name))
    })
    # }}}
    invisible(NULL)
}
