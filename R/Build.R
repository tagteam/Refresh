##' Build an R-package from source files.

#' @title R packaging tool combining emacs save buffers with
#'     devtools::document, devtools::build, devtools::install
#' @param pkg The quoted or unquoted name of the package
#' @param Source path to mother directory of the package source, defaults to options()$packageHome
#' @param ask If TRUE prompt user for source and library directories
#' @param quick passed to devtools::install
#' @param recompile use pkgbuild::compile_dll()?
#' @param quiet passed to devtools::install
#' @param vignettes if TRUE circumvent devtools::install and use R CMD build/INSTALL instead 
#' @param verbose blabla?
#' @param ... passed devtools::build
#' @param Source A list of directories or a single directory #' in
#'     which to search for the package's source code. Default is
#' \code{options()$refreshSource}.
#' @export
Build <- function(pkg,
                  Source=options()$packageHome,
                  lib=.libPaths()[1],
                  ask=FALSE,
                  quick=FALSE,
                  recompile=FALSE,
                  quiet=FALSE,
                  vignettes=TRUE,
                  verbose=TRUE,
                  ...){
    if (is.null(options()$packageHome)){
        message("You could set options()$packageHome in your .Rprofile to the folders on your computer\nin which the package directory with the source code of the package is found. E.g.,
     options(packageHome=c(\"~/R/dev/\",\"~/Software/\",\"~/myRpackages)\"")
    }
    system("emacsclient -e '(save-some-buffers)'", intern=TRUE)
    pkg <- as.character(substitute(pkg))
    oldPwd <- getwd()
    # {{{ Search for the uncompressed package directory 
    if (is.null(Source) || ask){
        cat("\nChoose the directory where the R source code of your package lives\n ")
        Source <- file.choose()
    }
    Source <- Source[!duplicated(Source)]
    found <- sapply(Source,function(s){file.exists(file.path(s,pkg,"DESCRIPTION"))})
    if (sum(found)>1){
        warning("Package source found in multiple places.")
        Spath <- select.list(Source[found],multiple=FALSE,title="Package source found in two different places, please choose: ")
        SourceP <- file.path(Spath,pkg)
    }
    else{
        if (any(found)) 
            SourceP <- file.path(Source[found],pkg)
        else
            stop("No directory with name '",pkg,"' found in:\n\n",paste(Source,"\n"))
    }
    # }}}
    lock <- paste(lib,"/00LOCK-",pkg,sep="")
    if (file.exists(lock)){
        if (verbose)
            message("Remove file:",lock)
        file.remove(lock)
    }
    
                                        # {{{ devtools::document, devtools::build, devtools::install
    setwd(file.path(SourceP))
    cat("\nRunning Rcpp::compileAttributes() ...\n")
    try(Rcpp::compileAttributes())
    if (recompile==TRUE){
        cat("\nRunning pkgbuild::compile_dll() ...\n")
        pkgbuild::compile_dll()
    }
    cat("\nRunning devtools::document() ...\n")
    devtools::document()
    if (vignettes==TRUE){
        cat("\nRunning pkgbuild::build() ...\n")
        tools::buildVignettes(dir=rprojroot::find_root("DESCRIPTION", "."))
        newpkg <- pkgbuild::build(needs_compilation=!quick)
        ## buildCMD <- paste(file.path(R.home(), "bin", "R"),"CMD build",SourceP)
        ## message(buildCMD)
        ## try(bbb <- system(buildCMD,intern=(verbose<2)),silent=TRUE)
        ## if (length(bbb)<9)
        ## cat("\n",rep("-",42),"\nMessages from build command\n",rep("-",42),"\n\n",sub("\\*","\\\n*",bbb),sep="")
    }else{
        cat("\nRunning devtools::build() ...\n")
        devtools::build(...,vignettes=FALSE)
    }
    if (vignettes==TRUE){
        run.install <- paste(file.path(R.home(),"bin","R"),"CMD INSTALL"," -l ",file.path(lib),newpkg)
        message(run.install)
        system(run.install,intern=(verbose<2))
        require(pkg, character.only = TRUE)
    }else{
        if (verbose){
            devtools::install(quick=quick,reload=TRUE,upgrade=FALSE,quiet=quiet,build_vignettes=FALSE)
        }else{
            cat("\nRunning devtools::install\nnot showing progress/output as this messes up the font-lock of the *R* buffer ...\n")
            capture.output(devtools::install(quick=quick,reload=TRUE,upgrade=FALSE,quiet=quiet,build_vignettes=FALSE))
        }
    }
                                        # }}}
    cat("\nCurrently installed version of",pkg,":\n")
    cat("\n",as.character(packageVersion(pkg)),"\n")
    setwd(oldPwd)
}


