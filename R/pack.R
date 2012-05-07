pack <- function(pkgName,
                 lib=options()$refreshLibrary,
                 Archive=options()$refreshArchive,
                 Source=options()$refreshSource,
                 ask=FALSE,
                 recursive=FALSE,
                 docs = TRUE,
                 vignettes = TRUE,
                 verbose=1){

  pkgName <- as.character(substitute(pkgName))
  oldPwd <- getwd()
  setwd(file.path(Archive))
  
  ## if (!is.character(pkgName))
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
  found <- sapply(Source,function(s){file.exists(file.path(s,pkgName))})
  ## print(file.path(Source,pkgName))
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

  if (!is.na(SourceP)){
    sourceCodeVersionLine <- system(paste("grep Version: ",file.path(SourceP,"DESCRIPTION")),intern=TRUE)
    sourceCodeVersion <- strsplit(sourceCodeVersionLine,"Version: ")[[1]][[2]]
  }
  else{
    ## stop if no directory with that name
    stop("No directory with name '",
         pkgName,
         "' found in:\n\n",
         paste(Source,"\n"))
  }
  # }}}
  # {{{  
  ##  Find the  source code and try to build the tarFile 
  ##  ------------------------------------------------------------------
  ## FIXME: instead of warn ask or backup or increase version number (using awk)
  allVersions <- list.files(path=file.path(Archive),pattern=paste(pkgName,".*.tar.gz",sep=""),recursive=recursive)
  newVersion <- paste(pkgName,"_",sourceCodeVersion,".tar.gz",sep="")
  if(match(newVersion,allVersions,nomatch=0)!=0)
    warning("Overwriting existing packaged version ",newVersion," in directory ",Archive)
  ## if (file.exists(file.path(Archive, freshVersion))){
  ## message(paste("\nCopied",file.path(Archive, freshVersion),"to",file.path(Archive,"old", freshVersion),"\n"))
  ## file.copy(file.path(Archive, freshVersion),file.path(Archive,"old", freshVersion))
  
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
  try(detach(pos=match(paste("package", pkgName, sep = ":"),
               search(),
               nomatch=FALSE),unload=TRUE),silent=TRUE)
  
  
  try(unloadNamespace(pkgName),silent=TRUE)
  dynname <- paste(file.path(lib),"/",pkgName,sep="")
  message(paste("try unloading ",dynname))
  try(library.dynam.unload(pkgName,dynname),silent=TRUE)
  #  TRUE(detach(paste("package", pkgName, sep = ":")),silent=T)
  
  setwd(oldPwd)

  # }}}
  # {{{ R-version specific install command
  ## check for lockfile
  lock <- paste(lib,"/00LOCK",sep="")
  ##   if (file.exists(lock)){
  if (verbose)
    message("Remove file:",lock)

  system(paste("rm -rf",lock),intern=(verbose<2))
  ## }

  if (verbose)
  cat("\n",rep("-",42),"\nInstalling the ",ifelse(is.na(SourceP),"selected","new")," version of ",pkgName,"\n",rep("-",42),"\n\n",sep="")
  
  run.install <- paste(file.path(R.home(),"bin","R"),
                       "CMD INSTALL",
                       if(docs != TRUE){'--no-docs'},
                       " -l ",
                       file.path(lib),
                       file.path(Archive, newVersion))
  message(run.install)
  system(run.install,intern=(verbose<2))
  require(pkgName, character.only = TRUE)
  # }}}
}



