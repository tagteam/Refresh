#' @export
check <- function(pkgName,
                  Archive=options()$refreshArchive,
                  LogDir=options()$refreshCheckLog,
                  lib=.libPaths()[1],
                  recursive=FALSE,
                  devel=file.exists("~/R/dev/R-devel"),
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
    if (devel==TRUE)
        R <- "~/R/dev/R-devel/bin/R"
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
  
