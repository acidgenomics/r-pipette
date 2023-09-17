## FIXME May need to provide internal methods for FTP and HTTPS here.
## FIXME Add support for listing files, symbolic links, dirs.
## "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
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
    keep <- grepl(pattern = "^d", x = lines)
    if (!any(keep)) {
        return(character())
    }
    lines <- lines[keep]
    ## FIXME Switch to using import once we add support for textConnection.
    ## "textConnection"
    df <- read.table(textConnection(lines))
    assert(identical(ncol(df), 9L))
    x <- sort(df[[9L]])
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
