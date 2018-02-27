##' Run R CMD check on a version of a package
##'
##' Run R CMD check on a version of a package
##' @title Run valgrind to find leaks in source code of a package 
##' @param pkgName name of package
#' @param Archive place on computer where the tar.gz versions of the package live 
#' @param LogDir place for output
#' @param lib R library 
#' @param recursive if \code{TRUE} search also subdirectories of \code{Archive} for package versions 
#' @param devel if \code{TRUE} use R's devel version
#' @param develR path to R's devel version
#' @param ask if \code{TRUE} let user choose a version
#' @param as.cran passed to R CMD check
#' @export
Check <- function(pkgName,
                  Archive=options()$refreshArchive,
                  LogDir=options()$refreshCheckLog,
                  lib=.libPaths()[1],
                  recursive=FALSE,
                  devel=FALSE,
                  develR="~/R/dev/R-devel/bin/R",
                  ask=FALSE,as.cran=TRUE){
    pkgName <- as.character(substitute(pkgName))  
    if (is.null(Archive) || ask)
        Archive <- file.choose()
    if (is.null(Archive) || !(file.exists(Archive)))
        warning("Argument `Archive' is not a valid directory-name.\nIt should be a directory in which the `packageName_version.tar.gz' files are stored.")
    ## make sure Archive does not end in /
    Archive <- sapply(Archive, function(a)strsplit(file.path(a),"/$")[[1]])
    ## check for lockfile
    if (version$major>=3 || (version$major>=2 & version$minor >= 15))
        lock <- paste(lib,"/00LOCK-",pkgName,sep="")
    else
        lock <- paste(lib,"/00LOCK",sep="")
    if (file.exists(lock)){
        message("Remove file:",lock)
        system(paste("rm -rf",lock),intern=FALSE)
    }
    versions <- list.files(path=file.path(Archive),
                           ignore.case = 1L,
                           pattern=paste(pkgName,".*.tar.gz$",sep=""),
                           recursive=recursive,full.names=TRUE)
    ## if (length(Archive)>1)
    ## Archive <- select.list(Archive,
    ## multiple=FALSE,
    ## title="Select Archive: ")
    version <- select.list(versions,
                           multiple=FALSE,
                           title="Select package version: ")
    if (!file.exists(LogDir)){
        cat("\nChoose a directory for the log of Rcheck (e.g. ~/tmp/) no quotes!\n")
        LogDir <- file.choose()
    }
    if (devel & file.exists(develR))
        R <- develR
    else
        R <- file.path(R.home(), "bin", "R")
    if (as.cran==TRUE)
        checkstring <- paste("cd ",
                             LogDir,
                             ";",
                             "export R_C_BOUNDS_CHECK='yes';",
                             R,
                             " CMD check --as-cran --library=",
                             path.expand(lib),
                             " ",
                             ## path.expand(file.path(Archive)),
                             ## "/",
                             version,
                             sep="")
    else
        checkstring <- paste("cd ",
                             LogDir,
                             ";",
                             "export R_C_BOUNDS_CHECK='yes';",
                             R,
                             " CMD check --library=",
                             path.expand(lib),
                             " ",
                             ## path.expand(file.path(Archive)),
                             ## "/",
                             version,
                             sep="")
    message(checkstring)
    system(checkstring)
}
  
