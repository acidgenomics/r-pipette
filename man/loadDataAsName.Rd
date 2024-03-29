% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadDataAsName.R
\name{loadDataAsName}
\alias{loadDataAsName}
\title{Load data as name}
\usage{
loadDataAsName(
  ...,
  dir = getOption(x = "acid.load.dir", default = getwd()),
  envir = globalenv(),
  overwrite = getOption(x = "acid.overwrite", default = TRUE)
)
}
\arguments{
\item{...}{Key value pairs, defining the name mappings. For example,
\code{newName1} = \code{oldName1}, \code{newName2} = \code{oldName2}. Note that these
arguments are interpreted using non-standard evaluation, and \emph{should not
be quoted}.}

\item{dir}{\code{character(1)}.
Directory path.}

\item{envir}{\code{environment}.
Environment.}

\item{overwrite}{\code{logical(1)}.
Overwrite existing file on disk.}
}
\value{
Invisible named \code{character}. File paths.
}
\description{
Load data as name
}
\note{
This function is intended for interactive use and interprets object
names using non-standard evaluation.

Updated 2023-06-29.
}
\examples{
dir <- system.file("extdata", package = "pipette")
loadDataAsName(renamed = example, dir = dir)
class(renamed)
}
