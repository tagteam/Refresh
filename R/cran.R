cran <- function(name,tarGet="~/R/packages",recursive=FALSE){
  versions <- list.files(path=file.path(tarGet),pattern=paste(name,".*.tar.gz",sep=""),recursive=recursive)
  version <- select.list(versions,multiple=FALSE,title="Select package version: ")
  cat("\nUsing program ncftp to upload the package:\n")
  ncftp.available <- system("ncftp -version")!=32512
  if (!ncftp.available)
    error("\nCould not run: ncftp --version ")
  else{
    uploadstring <- paste("ncftpput -u anonymous cran.r-project.org incoming ",path.expand(file.path(tarGet)),"/",version,sep="")
    doit <- select.list(c("yes","no"),multiple=FALSE,title=paste("Upload version",
                                                       uploadstring,
                                                       "\n???"))
    if (doit=="yes"){
      system(uploadstring)
      cat("\n**Now you might want to send an email to:**\n\nCRAN@R-project.org\n\nDear cRan executive director\n\nplease note my add-on package",version,"on\n\nftp://CRAN.R-project.org/incoming/\n\nBest regards,\n")
    }
  }
}
  
