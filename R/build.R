##' Build an R-package from source files.

#' @title R packaging tool controlled from within the running R process
#' @param pkg  The quoted or unquoted name of the package 
#' @param lib A directory in which to install the package. Default is
#' \code{options()$refreshLibrary}. The directory should be in \code{.libPaths()}.
#' @param Archive
#' A list of directories or a single directory
#' in which to archive the tar ball produced by CMD build. Default is \code{options()$refreshArchive}.
#' @param Source
#' A list of directories or a single directory
#' in which to search for the package's source code. Default is \code{options()$refreshSource}.
#' @param roxy if TRUE call roxygenize before building 
#' @param ask If TRUE prompt user for source and library directories
#' @param recursive
#' @param docs if set to FALSE add \code{--no-docs} to CMD install
#' @param vignettes if set to FALSE add \code{--no-vignettes} to CMD build
#' @param verbose The level of verbosity
#' @export
build <- function(pkg,
                  lib=options()$refreshLibrary,
                  Archive=options()$refreshArchive,
                  Source=options()$refreshSource,
                  roxy=TRUE,
                  ask=FALSE,
                  recursive=FALSE,
                  docs = TRUE,
                  vignettes = TRUE,
                  verbose=1){

  pkg <- as.character(substitute(pkg))
  oldPwd <- getwd()
  setwd(file.path(Archive))
  
  ## if (!is.character(pkg))
  # {{{  locating files
  
  if (missing(lib)) lib <- options()$refreshLibrary
  if (is.null(lib)||!(file.exists(lib)))
    warning("Argent lib is not a valid directory-name.\nYou may create such a location e.g. by evaluating the R-command\n `dir.create(\"~/R/library\",recursive=TRUE)'")
  lib <- path.expand(lib)
  if (!(match(sub("/$","",lib),sapply(.libPaths(),function(x)sub("/$","",x)),nomatch=FALSE)))
    warning("\nThe library is not in the search path.\nYou can add it by evaluating the R-command\n `.libPaths(\"",lib,"\")'")
  
  if (is.null(Archive) || ask){
    cat("\nChoose directory for saving the result of R CMD build\n ")
    Archive <- file.choose()
  }
  if (is.null(Archive) || !(file.exists(Archive)))
    warning("Argument `Archive' is not a valid directory-name.\nIt should be a directory in which the `packageName_version.tar.gz' files are stored.")

  # }}}
  # {{{  search for an uncompressed package directory with the source code

  if (is.null(Source) || ask){
    cat("\nChoose the directory where the R source code of your package lives\n ")
    Source <- file.choose()
  }
  
  Source <- Source[!duplicated(Source)]
  found <- sapply(Source,function(s){file.exists(file.path(s,pkg))})
  ## print(file.path(Source,pkg))
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

  if (!is.na(SourceP)){
      sourceCodeVersionLine <- system(paste("grep Version: ",file.path(SourceP,"DESCRIPTION")),intern=TRUE)
      sourceCodeVersion <- strsplit(sourceCodeVersionLine,"Version: ")[[1]][[2]]
  }
  else{
      ## stop if no directory with that name
      stop("No directory with name '",
           pkg,
           "' found in:\n\n",
           paste(Source,"\n"))
  }
  # }}}
  # {{{  
  ##  Find the  source code and try to build the tarFile 
  ##  ------------------------------------------------------------------
  ## FIXME: instead of warn ask or backup or increase version number (using awk)
  allVersions <- list.files(path=file.path(Archive),pattern=paste(pkg,".*.tar.gz",sep=""),recursive=recursive)
  newVersion <- paste(pkg,"_",sourceCodeVersion,".tar.gz",sep="")
  if(match(newVersion,allVersions,nomatch=0)!=0)
      warning("Overwriting existing packaged version ",newVersion," in directory ",Archive)
  ## if (file.exists(file.path(Archive, freshVersion))){
  ## message(paste("\nCopied",file.path(Archive, freshVersion),"to",file.path(Archive,"old", freshVersion),"\n"))
  ## file.copy(file.path(Archive, freshVersion),file.path(Archive,"old", freshVersion))
  if (roxy && require(roxygen2)){
      if (verbose)
          cat("... Roxygenizing ",pkg,"\n",sep="")
      roxygenize(SourceP)}
  
  buildCMD <- paste(file.path(R.home(), "bin", "R"),
                    "CMD build",
                    if(vignettes != TRUE){' --no-vignettes'},
                    SourceP)
  
  if (verbose>0)
    message("\nRefreshing (building) the package from the directory ",SourceP," via the command:\n\n",buildCMD,"\n")
    
    try(bbb <- system(buildCMD,intern=(verbose<2)),silent=TRUE)
  
  if (length(bbb)==0){
    cat("No such file or directory: ",SourceP)
  }
  else if (length(bbb)<9){
    cat("\n",rep("-",42),"\nMessages from build command\n",rep("-",42),"\n\n",sub("\\*","\\\n*",bbb),sep="")
  }

  # }}}
  # {{{  Unloading  
  try(detach(pos=match(paste("package", pkg, sep = ":"),
               search(),
               nomatch=FALSE),unload=TRUE),silent=TRUE)
  
  
  try(unloadNamespace(pkg),silent=TRUE)
  dynname <- paste(file.path(lib),"/",pkg,sep="")
  message(paste("try unloading ",dynname))
  try(library.dynam.unload(pkg,dynname),silent=TRUE)
  #  TRUE(detach(paste("package", pkg, sep = ":")),silent=T)
  
  setwd(oldPwd)

  # }}}
  # {{{ R-version specific install command
  ## check for lockfile
  if (version$major>=3 || (version$major>=2 & version$minor >= 15))
      lock <- paste(lib,"/00LOCK-",pkg,sep="")
  else
  lock <- paste(lib,"/00LOCK",sep="")
  ##   if (file.exists(lock)){
  if (verbose)
    message("Remove file:",lock)

  system(paste("rm -rf",lock),intern=(verbose<2))
  ## }

  if (verbose)
  cat("\n",rep("-",42),"\nInstalling the ",ifelse(is.na(SourceP),"selected","new")," version of ",pkg,"\n",rep("-",42),"\n\n",sep="")
  
  run.install <- paste(file.path(R.home(),"bin","R"),
                       "CMD INSTALL",
                       if(docs != TRUE){'--no-docs'},
                       " -l ",
                       file.path(lib),
                       file.path(Archive, newVersion))
  message(run.install)
  system(run.install,intern=(verbose<2))
  require(pkg, character.only = TRUE)
  # }}}
}



