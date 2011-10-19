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
  If the newly built package has the same version as one of the archived
  packages, then the archived package is moved to a directory Archive/old.

}
\value{
nil
}
\author{Thomas A. Gerds \email{tag@biostat.ku.dk}}
\seealso{\code{\link{Refresh-package}}}
\examples{
\dontrun{
refresh("roadrunner")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{package }
