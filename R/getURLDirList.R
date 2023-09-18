## FIXME May need to provide internal methods for FTP and HTTPS here.
## FIXME Add support for listing files, symbolic links, dirs.
## "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
## "https://ftp.ncbi.nlm.nih.gov/genomes/"
## FIXME Add option to return absolute path.

## goalie improvements:
## FIXME Add n support to hasCols.
## FIXME Add n support to hasRows.
## FIXME Add support for a specific dimension to hasDims.



#' Get remote URL directory listing
#'
#' @export
#' @note Updated 2023-09-17.
#'
#' @details
#' Currently, only FTP servers are supported.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param pattern `character(1)`.
#' Regular expression pattern to use for matching.
#' Passes to `grepl` function internally.
#'
#' @param absolute `logical(1)`.
#' Return absolute path.
#'
#' @return `character`.
#' Simple directory contents return, including both files and subdirectories.
#'
#' @seealso
#' - `curlGetHeaders`.
#'
#' @examples
#' url <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
#' if (goalie::isAnExistingURL(url)) {
#'     x <- getURLDirList(url)
#'     print(x)
#' }
getURLDirList <- function(url, pattern = NULL, absolute = FALSE) {
    assert(
        requireNamespaces("RCurl"),
        isString(url),
        ## FIXME Add support for http, https.
        isMatchingRegex(x = url, pattern = "^ftp://"),
        isString(pattern, nullOK = TRUE),
        isFlag(absolute)
    )
    if (!isMatchingRegex(x = url, pattern = "/$")) {
        url <- paste0(url, "/") # nocov
    }
    assert(isAnExistingURL(url))
    destfile <- tempfile()
    status <- try(
        expr = {
            download.file(
                url = url,
                destfile = destfile,
                quiet = TRUE
            )
        },
        silent = TRUE
    )
    assert(
        !inherits(status, "try-error"),
        msg = sprintf("URL failure: {.url %s}.", url)
    )
    lines <- import(destfile, format = "lines")
    unlink(destfile)
    x <- .ftpDirList(lines)
    if (!hasLength(x)) {
        return(character())
    }
    if (isString(pattern)) {
        keep <- grepl(pattern = pattern, x = x)
        assert(
            hasLength(keep),
            msg = sprintf(
                "No files matched pattern {.var %s}.",
                pattern
            )
        )
        x <- x[keep]
    }
    x
}



#' Get list of files from an FTP server
#'
#' @note Updated 2023-09-18.
#' @noRd
#'
#' @param x `character`.
#' Source code lines from `download.file`.
#'
#' @return `character`.
#' File and directory basenames.
.ftpDirList <- function(x) {
    keep <- grepl(pattern = "^d", x = x)
    if (!any(keep)) {
        return(character())
    }
    x <- x[keep]
    ## FIXME Switch to using import once we add support for textConnection.
    ## "textConnection"
    df <- read.table(textConnection(x))
    assert(identical(ncol(df), 9L))
    x <- sort(df[[9L]])
    x
}



## FIXME Support files/directories filtering.
## FIXME Support size filtering.

#' Get list of files from an HTTP(S) server
#'
#' @note Updated 2023-09-18.
#' @noRd
.httpDirList <- function(x) {
    if (!any(grepl(pattern = "^<h1>Index of", x = x))) {
        return(character())
    }
    pattern <- paste0(
        "^<a href=\".+\">(.+)</a>\\s+",
        "([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2})",
        "\\s+",
        "([0-9.]+[A-Za-z]+)",
        "\\s+",
        ".+$"
    )
    keep <- grepl(pattern = pattern, x = x)
    if (!any(keep)) {
        return(character())
    }
    x <- x[keep]
    x <- sub(
        pattern = pattern,
        replacement = "\"\\1\",\"\\2\",\"\\3\"",
        x = x,
        perl = TRUE
    )
    ## FIXME Need to add support for this in import.
    df <- read.csv(
        file = textConnection(x),
        header = FALSE,
        col.names = c("basename", "date", "size")
    )
    df[["date"]] <- as.POSIXlt(df[["date"]])
    ## FIXME How to convert to size??
    ## > df[["size"]]
    out <- df[["basename"]]
    out
}
