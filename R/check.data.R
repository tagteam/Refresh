check.data <- function(name,path="~/research/SoftWare",pattern=".R$"){
  name <- as.character(substitute(name))  
  path <- paste(dirname(path),"/",basename(path),"/",sep="")
  Rlib <- path.expand(paste(path,name,"/data",sep=""))
  print(Rlib)
  if (!file.exists(Rlib))
    stop(paste("File",Rlib," does not exist."))
  Rfiles <- list.files(path=file.path(Rlib),pattern=pattern)
  print(Rfiles)
  lapply(Rfiles,function(name){
    message(name)
    source(file.path(Rlib,name))
  })
  invisible(NULL)
}
