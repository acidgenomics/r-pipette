% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factorize-methods.R
\docType{methods}
\name{factorize}
\alias{factorize}
\alias{factorize,ANY-method}
\title{Factorize}
\usage{
\S4method{factorize}{ANY}(object)
}
\arguments{
\item{object}{Object.}
}
\value{
Modified object.
All columns will be coerced to \code{factor}.
}
\description{
Coerce all columns in the object to \code{factor} (i.e. \code{stringsAsFactors}).
}
\examples{
df <- S4Vectors::DataFrame(a = letters[seq_len(5)], b = seq_len(5))
x <- factorize(df)
}
\seealso{
\code{\link[=encode]{encode()}} for run-length encoding (Rle) approach.
}