\name{Refresh-package}
\alias{Refresh-package}
\alias{Refresh}
\docType{package}
\title{
  Rebuilding and checking of homegrown R packages.
}
\description{
  Functions for R package maintainers and other
  people who are too lazy and too oblivious to use R CMD build, install,
  check. In unix-like environments it the function cran will upload a
  package to CRAN and prepare a suitable email to the CRAN people.
}
\details{
refresh, check,  check.code, cran
}
\author{Thomas A. Gerds \email{tag@biostat.ku.dk}}
\keyword{package}
\examples{
\dontrun{
## 
# to use the full functionality of the Refresh package modify
# the following lines according to your own structure
# and add them to your .Rprofile:
##
# tell 'refresh' and R where to install the packages 
##
lib <- ifelse(.Machine$sizeof.pointer==4,"~/R/library","~/R/library64")
options(refreshLibrary=lib)
.libPaths(c(lib,.libPaths()))
##
# tell 'pack' and 'rollback' the place of archived package versions 
## 
options(refreshArchive="~/R/packages")
##
# tell 'refresh' and 'check.code' where to find the package sources
## 
options(refreshSource=c("~/R/iDevel","~/research/SoftWare"))
##
# tell 'check' where to place the log 
##
options(refreshLogDir="~/tmp")

# Create an empty package
# in one of the directories in options()$refreshSource, e.g.
setwd(options()$refreshSource[1])
coyote <- function(x)x
package.skeleton(list=c("coyote"),name="roadrunner")
# then edit the description file, add R code, data, man pages 

# run R CMD build and install:
refresh("roadrunner")

# if there is an error, locate the corresponding file:
check.code("roadrunner")

# run R CMD check
check("roadrunner")

# upload to cran:
cran("roadrunner")
}
}
