check <- function(pkgName,
                  Archive=options()$refreshArchive,
                  LogDir=options()$refreshCheckLog,
                  lib=.libPaths()[1],
                  recursive=FALSE,
                  ask=FALSE,as.cran=FALSE){
  pkgName <- as.character(substitute(pkgName))  
  if (is.null(Archive) || ask)
    Archive <- file.choose()
  if (is.null(Archive) || !(file.exists(Archive)))
    warning("Argument `Archive' is not a valid directory-name.\nIt should be a directory in which the `packageName_version.tar.gz' files are stored.")
  ## check for lockfile
  if (version$major>=2 & version$minor >= 15)
  lock <- paste(lib,"/00LOCK-",pkgName,sep="")
  else
  lock <- paste(lib,"/00LOCK",sep="")
  if (file.exists(lock)){
    message("Remove file:",lock)
    system(paste("rm -rf",lock),intern=FALSE)
  }
  versions <- list.files(path=file.path(Archive),pattern=paste(pkgName,".*.tar.gz$",sep=""),recursive=recursive)
  version <- select.list(versions,multiple=FALSE,title="Select package version: ")
  if (!file.exists(LogDir)){
    cat("\nChoose a directory for the log of Rcheck (e.g. ~/tmp/) no quotes!\n")
    LogDir <- file.choose()
  }
  if (as.cran==TRUE)
    checkstring <- paste("cd ",LogDir,";R --no-init-file CMD check --as-cran -l ",path.expand(lib)," ",path.expand(file.path(Archive)),"/",version,sep="")
  else
  checkstring <- paste("cd ",LogDir,";R CMD check -l ",path.expand(lib)," ",path.expand(file.path(Archive)),"/",version,sep="")
  message(checkstring)
  system(checkstring)
}
  
