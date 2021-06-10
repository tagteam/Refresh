##' Source all R files of a package 
##'
##' This is useful to try changes of a package and to identify syntax failures.
##' @aliases Source check.code
##' @title Source all R files of a package
##' @param pkg Quoted or unquoted name of the package
##' @param Source place where to look for package
##' @param cpp When \code{TRUE} run \code{Rcpp::sourceCpp} on all \code{.cpp} files in \code{src} directory. 
##' @param pattern Matched against file-names in package.
##' @return nothing
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
Source <- function(pkg,
                   Source=options()$packageHome,
                   cpp=FALSE,
                   pattern=".r$|.R$"){
    pkg <- as.character(substitute(pkg))
                                        # {{{  search for an uncompressed package directory with the source code
    Source <- Source[!duplicated(Source)]
    found <- sapply(Source,function(s){
        file.exists(file.path(s,pkg))
    })
    if (!any(found))
        stop("No directory ", pkg, " found")
    ## print(file.path(Source,pkg))
    system("emacsclient -e '(save-some-buffers t)'", intern=TRUE)
    if (sum(found)>1){
        ## restrict to those with a DESCRIPTION file
        Source <- sapply(Source[found],function(s){
            if (file.exists(paste(s,"/",pkg,"/DESCRIPTION",sep="")))
                s
            else NA
        })
        Source <- Source[!is.na(Source)]
        if (length(Source)>1){
            warning("Package source found in multiple different places.")
            Spath <- select.list(Source,multiple=FALSE,title="Select source directory: ")
            SourceDir <- file.path(Spath,pkg)
        }else{
            SourceDir <- file.path(Source,pkg)
        }
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
    Rfiles <- list.files(path=file.path(Rlib),pattern=pattern,ignore.case=1L)
    lapply(Rfiles,function(name){
        message(name)
        ## warning(name)
        source(file.path(Rlib,name))
    })
    if (cpp){
        clib <- path.expand(paste(SourceDir,"/src",sep=""))
        cppfiles <- list.files(path=file.path(clib),pattern=".cpp$",ignore.case=1L)
        lapply(cppfiles,function(name){
            message(name)
            ## warning(name)
            Rcpp::sourceCpp(file.path(clib,name))
        })
    }
                                        # }}}
    invisible(NULL)
}
