##' @export 
roxy <- function(pkg,
                 Source=options()$refreshSource,
                 ask=FALSE,
                 recursive=FALSE,
                 docs = TRUE,
                 vignettes = TRUE,
                 verbose=1){

    pkg <- as.character(substitute(pkg))
    oldPwd <- getwd()
    setwd(file.path(lib))
    # {{{  search for an uncompressed package directory with the source code
    if (is.null(Source) || ask){
        cat("\nChoose the directory where the R source code of your package lives\n ")
        Source <- file.choose()
    }
    Source <- Source[!duplicated(Source)]
    found <- sapply(Source,function(s){
        file.exists(file.path(s,pkg)) && file.exists(file.path(s,pkg,"DESCRIPTION"))
    })
    if (sum(found)>1){
        warning("Package source found in two different places.")
        Spath <- select.list(Source[found],multiple=FALSE,title="Package source found in two different places, please choose: ")
        SourceP <- file.path(Spath,pkg)
    }
    else{
        if (any(found)) 
            SourceP <- file.path(Source[found],pkg)
        else
            SourceP <- NA
    }
    # }}}
    # {{{ Running roxygenize
    require(roxygen2)
    if (verbose)
        cat("... Roxygenizing ",pkg,"\n",sep="")
    roxygenize(SourceP)
    # }}}
}
