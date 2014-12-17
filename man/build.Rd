\name{build}
\alias{build}
\title{R packaging tool controlled from within the running R process}
\usage{
build(pkg, lib = options()$refreshLibrary,
  Archive = options()$refreshArchive, Source = options()$refreshSource,
  roxy = FALSE, ask = FALSE, recursive = FALSE, docs = TRUE,
  vignettes = TRUE, devel = FALSE, verbose = 2)
}
\arguments{
  \item{pkg}{The quoted or unquoted name of the package}

  \item{lib}{A directory in which to install the package.
  Default is \code{options()$refreshLibrary}. The directory
  should be in \code{.libPaths()}.}

  \item{Archive}{A list of directories or a single
  directory in which to archive the tar ball produced by
  CMD build. Default is \code{options()$refreshArchive}.}

  \item{Source}{A list of directories or a single directory
  in which to search for the package's source code. Default
  is \code{options()$refreshSource}.}

  \item{roxy}{if TRUE call roxygenize before building}

  \item{ask}{If TRUE prompt user for source and library
  directories}

  \item{recursive}{If TRUE search for the package in
  subdirectories of \code{Archive}.}

  \item{docs}{if set to FALSE add \code{--no-docs} to CMD
  install}

  \item{vignettes}{if set to FALSE add
  \code{--no-vignettes} to CMD build}

  \item{verbose}{The level of verbosity}
}
\description{
Build an R-package from source files.
}

