\name{refresh}
\alias{build}
\alias{check}
\alias{cran}
\alias{refresh}
\alias{rollback}
\alias{roxy}
\title{R packaging tools controlled from within the running R process}
\usage{
refresh(pkg, lib = options()$refreshLibrary,
  Source = options()$refreshSource, roxy = FALSE, ask = FALSE,
  recursive = FALSE, docs = TRUE, vignettes = TRUE, verbose = 2)
}
\arguments{
  \item{pkg}{The quoted or unquoted name of the package}

  \item{lib}{A directory in which to install the package.
  Default is \code{options()$refreshLibrary}. The directory
  should be in \code{.libPaths()}.}

  \item{Source}{A list of directories or a single directory
  where to find the package source code. Default is
  \code{options()$refreshSource}}

  \item{ask}{If \code{TRUE} ask for Archive and Source
  directories.}

  \item{roxy}{if TRUE call roxygenize before building}

  \item{recursive}{If \code{TRUE} search in subdirectories
  of \code{Archive}.}

  \item{docs}{If \code{FALSE} add flag \code{" --no-docs"}
  to the R CMD install command.}

  \item{vignettes}{If \code{FALSE} add flag \code{"
  --no-vignettes"} to the R CMD build command.}

  \item{verbose}{Decides about the degree of talkitivity.}
}
\value{
Nothing
}
\description{
You have changed a tiny detail in a man page of your R
package, or rewritten the main function of your R package,
then you want to re-build and re-install the package. This
function is for lazy people like me who do not each time
want to open a terminal, find the syntax "R CMD ...",
restart R. Enjoy.
}
\examples{
refresh(Refresh)

\dontrun{

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

# and by calling 'check.code' which runs all R-files of the package
# via 'source(filename)'

check.code(roadrunner)}
}
\author{
Thomas A. Gerds <tag@biostat.ku.dk>
}
\seealso{
build
}
\keyword{package}

