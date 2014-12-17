\name{install}
\alias{install}
\alias{rforge}
\title{Install an R package}
\usage{
install(pkg, ...)
}
\arguments{
  \item{pkg}{Quoted or unquoted name of the package}

  \item{...}{passed to install.packages}
}
\value{
see install.packages
}
\description{
Install an R package
}
\details{
This is a wrapper for install.packages which allows the
user to forget the quotes. Also, the package is loaded
after installation.
}
\examples{
install(prodlim)
}
\author{
Thomas A. Gerds <tag@biostat.ku.dk>
}
\seealso{
install.packages
}

