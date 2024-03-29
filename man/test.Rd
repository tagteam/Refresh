% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test.R
\name{test}
\alias{test}
\title{Running test files}
\usage{
test(pkgName, Source = options()$packageHome, examples = FALSE, verbose = 1)
}
\arguments{
\item{pkgName}{Name of package}

\item{Source}{directory in which the package source code lives}

\item{examples}{if TRUE run examples including dontrun}

\item{verbose}{jabber or not}
}
\value{
nothing
}
\description{
Running test files
}
\details{
Source all files in pkg/tests and run examples
}
\examples{
\dontrun{
test(prodlim)
}
}
\author{
Thomas A. Gerds
}
