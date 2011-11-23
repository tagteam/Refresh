rollback <- function(pkgName,
                     lib=options()$refreshLibrary,
                     Archive=options()$refreshArchive,
                     ask=FALSE,
                     recursive=FALSE,
                     docs = TRUE,
                     vignettes = TRUE,
                     verbose=1){
  pkgName <- as.character(substitute(pkgName))  
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
  # {{{  search for existing versions of the package
  
  oldPwd <- getwd()
  setwd(file.path(Archive))
  
  tarGz.versions <- list.files(path=file.path(Archive),pattern=paste(pkgName,".*.tar.gz",sep=""),recursive=recursive)
  zip.versions <- list.files(path=file.path(Archive),pattern=paste(pkgName,".*.zip",sep=""),recursive=recursive)
  
  # }}}
  # {{{  now decide what to do
  availableVersions <- c(zip.versions,tarGz.versions)
  if (length(availableVersions)==0){
    stop("\n\nNo compressed package versions found in\n",
         paste(Archive,"\n"))
  }
  selectedVersion <- select.list(availableVersions,multiple=FALSE,title="Select package version: ")
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
  cat("\n",rep("-",42),"\nInstalling the selected version of ",pkgName,"\n",rep("-",42),"\n\n",sep="")
  
  run.install <- paste(file.path(R.home(),"bin","R"),
                       "CMD INSTALL",
                       if(docs != TRUE){'--no-docs'},
                       " -l ",
                       file.path(lib),
                       file.path(Archive, selectedVersion))
  message(run.install)
  system(run.install,intern=(verbose<2))
  require(pkgName, character.only = TRUE)
  # }}}
}

