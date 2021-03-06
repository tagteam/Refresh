##' Running test files 
##'
##' Source all files in pkg/tests and run examples
##' @title Running test files
##' @param pkgName Name of package
##' @param Source directory in which the package source code lives
##' @param examples if TRUE run examples including dontrun 
##' @param verbose jabber or not
##' @return nothing
##' @author Thomas A. Gerds
##' @examples
##' \dontrun{
##' test(prodlim)
##'}
##' @export
test <- function(pkgName,
                 Source=options()$packageHome,
                 examples=FALSE,
                 verbose=1){
    pkgName <- as.character(substitute(pkgName))
    Source <- Source[!duplicated(Source)]
    found <- sapply(Source,function(s){
        file.exists(file.path(s,pkgName))
    })
    if (!any(found))
        stop("No directory ", pkgName, " found")
    ## print(file.path(Source,pkg))
    if (sum(found)>1){
        ## restrict to those with a DESCRIPTION file
        Source <- sapply(Source[found],function(s){
            if (file.exists(paste(s,"/",pkgName,"/DESCRIPTION",sep="")))
                s
            else NA
        })
        Source <- Source[!is.na(Source)]
        if (length(Source)>1){
            warning("Package source found in multiple different places.")
            Spath <- select.list(Source,multiple=FALSE,title="Select source directory: ")
            SourceDir <- file.path(Spath,pkgName)
        }else{
            SourceDir <- file.path(Source,pkgName)
        }
    }
    else{
        if (any(found)) 
            SourceDir <- file.path(Source[found],pkgName)
        else
            SourceDir <- NA
    }
    # {{{ source the test files
    testlib <- path.expand(paste(SourceDir,"/tests",sep=""))
    if (!file.exists(testlib)){
        warning(paste("File",testlib," does not exist."))
    }else{
        testfiles <- list.files(path=file.path(testlib),recursive=TRUE,pattern=".r$|.R$",ignore.case=1L)
        alltime <- lapply(testfiles,function(name){
            message(name)
            this.user <- system("echo $USER",intern=TRUE)
            system("USER=tester",intern=TRUE)
            xtime <- system.time(source(file.path(testlib,name)))
            system(paste0("USER=",this.user),intern=TRUE) 
            message(paste0("Seconds: ",round(xtime[[3]],1)))
            xtime[3]
        })
        message(paste0("Total time: ",round(sum(unlist(alltime)),1)))
    }
    # }}}
    # {{{ running examples
    if (examples){
        pos <- match(paste("package:",pkgName,sep=""),search(),nomatch=0)
        if (pos==0) stop("Package not in search() path.")
        funs <- ls(pos)
        do.call("require",list(pkgName))
        pdf.name <- paste("/tmp/",pkgName,"-examples.pdf",sep="")
        pdf(pdf.name)
        par(ask=FALSE)
        plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
        text(0,0,paste("Collection of plots\ngenerated by the example\nsections of package:\n",pkgName,sep=""),cex=2)
        for (f in funs){
            print(f)
            plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
            text(0,0,paste("Collection of plots\ngenerated by the example\nsections of function:\n",f,sep=""),cex=2)        
            try(do.call(example,list(f,ask=0L,run.dontrun=1L,run.donttest=0L)),silent=TRUE)
        }
        dev.off()
        if (file.exists(pdf.name))
            system(paste("evince",pdf.name,"&"))
    }
    # }}}
}
    
