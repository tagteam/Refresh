roxy <- function(pkgName,
                 Source=options()$refreshSource,
                 ask=FALSE,
                 recursive=FALSE,
                 docs = TRUE,
                 vignettes = TRUE,
                 verbose=1){

    pkgName <- as.character(substitute(pkgName))
    oldPwd <- getwd()
    setwd(file.path(lib))
    # {{{  search for an uncompressed package directory with the source code
    if (is.null(Source) || ask){
        cat("\nChoose the directory where the R source code of your package lives\n ")
        Source <- file.choose()
    }
    Source <- Source[!duplicated(Source)]
    found <- sapply(Source,function(s){
        file.exists(file.path(s,pkgName)) && file.exists(file.path(s,pkgName,"DESCRIPTION"))
    })
    if (sum(found)>1){
        warning("Package source found in two different places.")
        Spath <- select.list(Source[found],multiple=FALSE,title="Package source found in two different places, please choose: ")
        SourceP <- file.path(Spath,pkgName)
    }
    else{
        if (any(found)) 
            SourceP <- file.path(Source[found],pkgName)
        else
            SourceP <- NA
    }
    # }}}
    # {{{ Running roxygenize
    require(roxygen2)
    if (verbose)
        cat("\n","... Roxygenizing ",pkgName,"\n",sep="")
    roxygenize(SourceP)
    # }}}
}
