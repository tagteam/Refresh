check.code <- function(pkgName,
                       Source=options()$refreshSource,
                       pattern=".r$|.R$"){
  pkgName <- as.character(substitute(pkgName))  
  # {{{  search for an uncompressed package directory with the source code
  Source <- Source[!duplicated(Source)]
  found <- sapply(Source,function(s){file.exists(file.path(s,pkgName))})
  if (!any(found))
    stop("No directory ", pkgName, " found")
  ## print(file.path(Source,pkgName))
  if (sum(found)>1){
    warning("Package source found in two different places.")
    Spath <- select.list(Source[found],multiple=FALSE,title="Select source directory: ")
    SourceDir <- file.path(Spath,pkgName)
  }
  else{
    if (any(found)) 
      SourceDir <- file.path(Source[found],pkgName)
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
