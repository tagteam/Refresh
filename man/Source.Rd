\name{Source}
\alias{Source}
\alias{check.code}
\title{Source all R files of a package}
\usage{
Source(pkg, Source = options()$refreshSource, pattern = ".r$|.R$")
}
\arguments{
  \item{pkg}{Quoted or unquoted name of the package}

  \item{Source}{place where to look for package}

  \item{pattern}{Matched against file-names in package.}
}
\value{
nothing
}
\description{
Source all R files of a package
}
\details{
This is useful to try changes of a package and to identify
syntax failures.
}
\author{
Thomas A. Gerds <tag@biostat.ku.dk>
}

