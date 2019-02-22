##' Build an R-package from source files.

#' @title R packaging tool combining emacs save buffers with devtools::document, devtools::build, devtools::install
#' @param pkg The quoted or unquoted name of the package 
#' @param Source
#' A list of directories or a single directory
#' in which to search for the package's source code. Default is \code{options()$refreshSource}.
#' @param ask If TRUE prompt user for source and library directories
#' @param vignettes passed to devtools::install as build_vignettes
#' @param ... passed devtools::build
#' @export
Build <- function(pkg,
                  Source=options()$packageHome,
                  ask=FALSE,
                  quick=FALSE,
                  recompile=FALSE,
                  quiet=TRUE,
                  vignettes=FALSE,
                  ...){
    if (is.null(options()$packageHome)){
        stop("You should set options()$packageHome in your .Rprofile to the folders on your computer\nin which the package directory with the source code of the package is found. E.g.,
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
        warning("Package source found in two different places.")
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
    # {{{ devtools::document, devtools::build, devtools::install
    setwd(file.path(SourceP))   
    cat("\nRunning devtools::build_vignettes() ...\n")
    if (vignettes)
        devtools::build_vignettes()
    cat("\nRunning Rcpp::compileAttributes() ...\n")
    Rcpp::compileAttributes()
    cat("\nRunning devtools::document() ...\n")
    devtools::document()
    if (recompile==TRUE)
        pkgbuild::compile_dll()
    cat("\nRunning devtools::build() ...\n")
    devtools::build(...,vignettes=FALSE)
    cat("\n",rep("-",options()$width),"\nInstalling the ",ifelse(is.na(SourceP),"selected","new")," version of ",pkg,"\n",rep("-",options()$width),"\n",sep="")
    cat("\nRunning devtools::install ...\n")
    devtools::install(quick=quick,reload=TRUE, upgrade=FALSE, quiet=quiet,build_vignettes=vignettes)
    # }}}
    cat("\nCurrently installed version of",pkg,":\n")
    cat("\n",as.character(packageVersion(pkg)),"\n")
    setwd(oldPwd)
}


