cran <- function(name,tarGet="~/R/packages",recursive=FALSE,email=FALSE){
    name <- as.character(substitute(name))  
    versions <- list.files(path=file.path(tarGet),pattern=paste(name,".*.tar.gz",sep=""),recursive=recursive)
    version <- select.list(versions,multiple=FALSE,title="Select package version: ")
    cat("\nUsing program ncftp to upload the package:\n")
    ncftp.available <- system("ncftp -version")!=32512
    if (!ncftp.available)
        error("\nCould not find program: ncftp --version ")
    else{
        uploadstring <- paste("ncftpput -u anonymous cran.r-project.org incoming ",path.expand(file.path(tarGet)),"/",version,sep="")
        doit <- select.list(c("yes","no"),multiple=FALSE,title=paste("Upload version",
                                                             uploadstring,
                                                             "\n???"))
        if (doit=="yes"){
            system(uploadstring)
        }
        if (email=="emacsclient"){
            system(paste("/usr/bin/emacsclient -e '(progn (select-frame (new-frame))",
                         "(gnus-group-mail) (insert \"CRAN@R-project.org\")",
                         "(beginning-of-line)",
                         "(message-goto-subject)",
                         "(insert \"CRAN submission", name, unlist(strsplit(version,"_|tar.gz"))[2],"\")",
                         "(message-goto-body)",
                         "(insert \"","\nDear cRan executive team\n",
                         "\nplease note the new version of my R-package",
                         version,
                         "on\n\nftp://CRAN.R-project.org/incoming/\n\n",
                         "I have read and agree to these policies: http://cran.r-project.org/web/packages/policies.html\n\n",
                         "Thank you very much for lifting it onto cRan!\n",
                         "\nBest regards,\n","\"))'}"))
        } else{
            cat("\n**Now you might want to send an email to:",
                "**\n\nTo: CRAN@R-project.org\n",
                "With the following subject line:\n",
                "\nSubject:",
                "CRAN submission",
                name,
                unlist(strsplit(version,"_|tar.gz"))[2],
                "\n",
                "\nDear cRan executive director\n",
                "\nplease note the new version of my R-package",
                version,
                "on\n\nftp://CRAN.R-project.org/incoming/\n\n",
                "I have read and agree to these policies: http://cran.r-project.org/web/packages/policies.html\n\n",
                "Thank you very much for lifting it onto cRan!\n",
                "\nBest regards,\n")
        }
    }
}
  
