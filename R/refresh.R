##'   You have changed a tiny detail in a man page of your R package, or
##'  rewritten the main function of your R package, then you want to
##'  re-build and re-install the package. This function is for lazy people
##'  like me who do not each time want to open a terminal, find the syntax
##'  "R CMD ...", restart R. Enjoy.
##'
##' 
##' @title R packaging tools controlled from within the running R process
##' @aliases roxy build rollback check cran 
##' @param pkg The quoted or unquoted name of the package 
##' @param lib     A directory in which to install the package. Default is
##' \code{options()$refreshLibrary}. The directory should be in \code{.libPaths()}.
##' @param Source A list of directories or a single directory
##' where to find the package source code. Default is
##' \code{options()$refreshSource}
##' @param ask If \code{TRUE} ask for Archive and Source
##' directories.
##' @param roxy if TRUE call roxygenize before building 
##' @param recursive If \code{TRUE} search in subdirectories of \code{Archive}. 
##' @param docs If \code{FALSE}  add flag \code{" --no-docs"} to
##' the R CMD install command. 
##' @param vignettes If \code{FALSE}  add flag \code{" --no-vignettes"}
##' to the R CMD build command. 
##' @param verbose Decides about the degree of talkitivity.
##' @return Nothing
##' @seealso build 
##' @examples
##'
##' \dontrun{
##' refresh(Refresh)
##'
##'# Suppose you have an R-package called 'roadrunner'.
##'# 
##'# (Recall that an R-package is just a directory in your
##'# file-system which includes a valid DESCRIPTION file
##'# and sub-directories called 'R', 'man', 'data', 'example')
##'#
##'# Suppose further that the R-package 'roadrunner' is
##'# a subdirectory of the directory '~/R/iDevel/'
##'# Then, the following command will try to install the
##'# package to the subdirectory '~/R/library'  
##'##
##'
##'refresh("roadrunner",lib="~/R/library",Source="~/R/iDevel/")
##'
##'##
##'# If this works, then you may want to customize for future
##'# usage. Add the following lines to your .Rprofile:
##'##
##'lib <- ifelse(.Machine$sizeof.pointer==4,"~/R/library","~/R/library64")
##'options(refreshLibrary=lib)
##'.libPaths(c(lib,.libPaths()))
##'options(refreshSource=c("~/R/iDevel"))
##'##
##'# Then it is easy to refresh the package:
##'##
##'
##'refresh("roadrunner")
##'
##'# or even without quotes
##'
##'refresh(roadrunner)
##'
##'
##'##
##'# If the refresh failes, you may want to start investigating
##'# why by telling 'refresh' be more talkative 
##'##
##'refresh(roadrunner,verbose=2)
##'
#'# and by calling 'Source' which runs all R-files of the package
#'# via 'source(filename)'
##'
##' Source(roadrunner)}
##' @keywords package 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
##' @export 
refresh <- function(pkg,
                    lib=options()$refreshLibrary,
                    ## Archive=options()$refreshArchive,
                    Source=options()$refreshSource,
                    roxy=FALSE,
                    ask=FALSE,
                    recursive=FALSE,
                    docs = TRUE,
                    vignettes = TRUE,
                    verbose=2){

  pkg <- as.character(substitute(pkg))
  oldPwd <- getwd()
  setwd(file.path(lib))
  ## setwd(file.path(Archive))
  ## FIXME
  ## roxygenize("/home/ifsv/grb615/research/SoftWare/lava/")  
  ## if (!is.character(pkg))
  # {{{  locating files
  
  if (missing(lib)) lib <- options()$refreshLibrary
  if (is.null(lib)||!(file.exists(lib)))
    warning("Argent lib is not a valid directory-name.\nYou may create such a location e.g. by evaluating the R-command\n `dir.create(\"~/R/library\",recursive=TRUE)'")
  lib <- path.expand(lib)
  if (!(match(sub("/$","",lib),sapply(.libPaths(),function(x)sub("/$","",x)),nomatch=FALSE)))
    warning("\nThe library is not in the search path.\nYou can add it by evaluating the R-command\n `.libPaths(\"",lib,"\")'")
  ## if (is.null(Archive) || ask){
    ## cat("\nChoose directory for saving the result of R CMD build\n ")
    ## Archive <- file.choose()
  ## }
  ## if (is.null(Archive) || !(file.exists(Archive)))
    ## warning("Argument `Archive' is not a valid directory-name.\nIt should be a directory in which the `packageName_version.tar.gz' files are stored.")

  # }}}
  # {{{  search for an uncompressed package directory with the source code
  if (is.null(Source) || ask){
    cat("\nChoose the directory where the R source code of your package lives\n ")
    Source <- file.choose()
  }
  
  Source <- Source[!duplicated(Source)]
  found <- sapply(Source,function(s){
      file.exists(file.path(s,pkg)) && file.exists(file.path(s,pkg,"DESCRIPTION"))
  })
  ## print(file.path(Source,pkg))
  if (sum(found)>1){
      warning("Package source found in two different places.")
      Spath <- select.list(Source[found],multiple=FALSE,title="Package source found in two different places, please choose: ")
      SourceP <- file.path(Spath,pkg)
  }
  else{
      if (any(found)){
          SourceP <- file.path(Source[found],pkg)
          message("Package found here:", SourceP)

      }
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
         "' which includes a DESCRIPTION file found in:\n\n",
         paste(Source,"\n"))
  }
  # }}}
  # {{{ building  
  ##  Find the  source code and try to build the tarFile 
  ##  ------------------------------------------------------------------
  ## FIXME: instead of warn ask or backup or increase version number (using awk)
  ## allVersions <- list.files(path=file.path(Archive),pattern=paste(pkg,".*.tar.gz",sep=""),recursive=recursive)
  ## newVersion <- paste(pkg,"_",sourceCodeVersion,".tar.gz",sep="")
  ## if(match(newVersion,allVersions,nomatch=0)!=0)
    ## warning("Overwriting existing packaged version ",newVersion," in directory ",Archive)
  ## if (file.exists(file.path(Archive, freshVersion))){
  ## message(paste("\nCopied",file.path(Archive, freshVersion),"to",file.path(Archive,"old", freshVersion),"\n"))
  ## file.copy(file.path(Archive, freshVersion),file.path(Archive,"old", freshVersion))
  
  ## buildCMD <- paste(file.path(R.home(), "bin", "R"),
                    ## "CMD build",
                    ## if(vignettes != TRUE){' --no-vignettes'},
                    ## SourceP)
  
  ## if (verbose>0)
    ## message("\nRefreshing (building) the package from the directory ",SourceP," via the command:\n\n",buildCMD,"\n")
    
    ## try(bbb <- system(buildCMD,intern=(verbose<2)),silent=TRUE)
  
  ## if (length(bbb)==0){
  ## cat("No such file or directory: ",SourceP)
  ## }
  ## else if (length(bbb)<9){
  ## cat("\n",rep("-",42),"\nMessages from build command\n",rep("-",42),"\n\n",sub("\\*","\\\n*",bbb),sep="")
  ## }
  
  # }}}
  # {{{ removing .o files
  if (file.exists(file.path(SourceP,"src"))){
    ofiles <- list.files(file.path(SourceP,"src"),pattern="\\.o$",ignore.case=1L)
    if (length(ofiles)>0){
      ## if (select.list(list("y","n"),multiple=FALSE,title=paste("Remove ofiles?:\n ",paste(ofiles,collapse="\n"),"Remove ofiles? "))=="y"){
      message("remove .o files")
      file.remove(sapply(ofiles,function(f){file.path(SourceP,"src",f)}))
      ## }
    }
  }
  # }}}
  ## require(devtools)
  ## devtools:::compile_rcpp_attributes(SourceP)
  # {{{  Unloading  

  try(detach(pos=match(paste("package", pkg, sep = ":"),
               search(),
               nomatch=FALSE),unload=TRUE),silent=TRUE)
  
  
  try(unloadNamespace(pkg),silent=TRUE)
  dynname <- paste(file.path(lib),"/",pkg,sep="")
  message(paste("try unloading ",dynname))
  try(library.dynam.unload(pkg,dynname),silent=TRUE)
  
  setwd(oldPwd)

    # }}}
    # {{{ R-version specific install command
    ## check for so files
    ## check for so files
    ## sofile <- list.files(path=file.path(Source,"src"),pattern=paste(pkg,"\\.so$",sep=""))
    ## if (length(sofile)>0){
        ## for (so in sofile){
            ## if (verbose) message("Remove file:",so)
            ## system(paste("rm -rf",so),intern=(verbose<2))
        ## }
    ## }
    ## check for lockfile
    lock <- paste(lib,"/00LOCK-",pkg,sep="")
    if (file.exists(lock)){
        if (verbose)
            message("Remove file:",lock)
        system(paste("rm -rf",lock),intern=(verbose<2))
    }
  
  if (roxy){
      if (verbose)
          cat("... Roxygenizing ",pkg,"\n",sep="")
      roxygen2::roxygenize(SourceP)
  }
  
  if (verbose)
  cat("\n",rep("-",42),"\nInstalling the ",ifelse(is.na(SourceP),"selected","new")," version of ",pkg,"\n",rep("-",42),"\n\n",sep="")
  
  run.install <- paste(file.path(R.home(),"bin","R"),
                       "CMD INSTALL",
                       if(docs != TRUE){'--no-docs'},
                       " -l ",
                       file.path(lib),
                       SourceP)
                       ## file.path(Archive, newVersion) )
  message(run.install)
  system(run.install,intern=(verbose<2))
  require(pkg, character.only = TRUE)

  # }}}
}
