% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checksums.R
\name{checksums}
\alias{checksums}
\alias{md5}
\alias{sha256}
\title{Generate checksum from file}
\usage{
md5(file)

sha256(file)
}
\arguments{
\item{file}{\code{character(1)}.
File path.}
}
\description{
Generate checksum from file
}
\details{
Requires digest package to be installed.
}
\note{
Updated 2023-09-14.
}
\examples{
file <- system.file("extdata/example.csv", package = "pipette")
md5(file)
sha256(file)
}
\seealso{
\itemize{
\item \code{digest::digest}.
}
}
