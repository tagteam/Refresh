##' @export 
rforge <- function(pkg,...){
  pkg <- as.character(substitute(pkg))  
  install.packages(pkg,repos="http://R-Forge.R-project.org")
}
