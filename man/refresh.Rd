\name{refresh}
\alias{refresh}
\title{Re-build and re-install an R package from within a running R process.}
\description{
  You have changed a tiny detail in a man page of your R package, or
  rewritten the main function of your R package, then you want to
  re-build and re-install the package. This function is for lazy people
  like me who do not each time want to open a terminal, find the syntax
  "R CMD ...", restart R. Enjoy.
}
\usage{
refresh(pkgName,
	lib=options()$refreshLibrary,
	Archive=options()$refreshArchive,
	Source=options()$refreshSource,
	ask=FALSE,
	recursive=FALSE,
	docs = TRUE,
	vignettes = TRUE,
	verbose=1)}
\arguments{
  \item{pkgName}{The name of the package (character).}
  \item{lib}{A directory in which to install the package. Default is
    \code{options()$refreshLibrary}. The directory should be in \code{.libPaths()}.
  }
  \item{Archive}{A directory in which old versions of the package are
    stored and in which to save the result of R CMD
    build. Default is \code{options()$refreshArchive}}
  \item{Source}{A list of directories or a single directory where to
    find the package source code. Default is
    \code{options()$refreshSource}
  }
  \item{ask}{If \code{TRUE} ask for Archive and Source directories. }
  \item{recursive}{If \code{TRUE} search in subdirectories of Archive. }
  \item{docs}{If \code{FALSE}  add flag \code{" --no-docs"} to
    the R CMD install command. }
  \item{vignettes}{If \code{FALSE}  add flag \code{" --no-vignettes"} to the R CMD build command. }
}
\details{
  \code{refresh} does not build the archive, i.e. the package-version.tar.gz 
  or package-version.zip archive, but the function \code{pack} can do this.
}
\value{
nil
}
\author{Thomas A. Gerds \email{tag@biostat.ku.dk}}
\seealso{\code{\link{Refresh-package}}}
\examples{
\dontrun{
##
# Suppose you have an R-package called 'roadrunner'.
# 
# (Recall that an R-package is just a directory in your
# file-system which includes a valid DESCRIPTION file
# and sub-directories called 'R', 'man', 'data', 'example')
#
# Suppose further that the R-package 'roadrunner' is
# a subdirectory of the directory '~/R/iDevel/'
# Then, the following command will try to install the
# package to the subdirectory '~/R/library'  
##

refresh("roadrunner",lib="~/R/library",Source="~/R/iDevel/")

##
# If this works, then you may want to customize for future
# usage. Add the following lines to your .Rprofile:
##
lib <- ifelse(.Machine$sizeof.pointer==4,"~/R/library","~/R/library64")
options(refreshLibrary=lib)
.libPaths(c(lib,.libPaths()))
options(refreshSource=c("~/R/iDevel"))
##
# Then it is easy to refresh the package:
##

refresh("roadrunner")

# or even without quotes

refresh(roadrunner)

##
# If the refresh failes, you may want to start investigating
# why by telling 'refresh' be more talkative 
##
refresh(roadrunner,verbose=2)

## and by calling 'check.code' which runs all R-files of the package
## via 'source(filename)'

check.code(roadrunner)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{package }
