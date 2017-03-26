##' @export 
windoof <- function(pgk,tarGet="~/R/packages",recursive=FALSE){
  pgk <- as.character(substitute(pgk))  
  versions <- list.files(path=file.path(tarGet),pattern=paste(pgk,".*.tar.gz",sep=""),recursive=recursive,ignore.case=1L)
  version <- select.list(versions,multiple=FALSE,title="Select package version: ")
  cat("\nUsing program ncftp to upload the package:\n")
  ncftp.available <- system("ncftp -version")!=32512
  if (!ncftp.available)
    error("\nCould not find program: ncftp --version ")
  else{
    uploadstring <- paste("ncftpput -u anonymous win-builder.r-project.org incoming ",path.expand(file.path(tarGet)),"/",version,sep="")
    doit <- select.list(c("yes","no"),multiple=FALSE,title=paste("Upload version",
                                                       uploadstring,
                                                       "\n???"))
    if (doit=="yes"){
      system(uploadstring)
    }
  }
}
  
## ftp://win-builder.r-project.org/
## http://win-builder.r-project.org/
