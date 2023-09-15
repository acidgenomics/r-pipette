#' Get remote URL directory listing
#'
#' @export
#' @note Updated 2023-09-15.
#'
#' @details
#' Requires RCurl package to be installed.
#' Currently, only FTP servers are supported.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param pattern `character(1)`.
#' Regular expression pattern to use for matching.
#' Passes to `grepl` function internally.
#'
#' @return `character`.
#' Simple directory contents return, including both files and subdirectories.
#'
#' @examples
#' url <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
#' if (goalie::isAnExistingURL(url)) {
#'     x <- getURLDirList(url)
#'     tail(x)
#' }
getURLDirList <- function(url, pattern = NULL) {
    assert(
        requireNamespaces("RCurl"),
        isString(url),
        isString(pattern, nullOK = TRUE),
        isMatchingRegex(x = url, pattern = "^ftp://")
    )
    if (!isMatchingRegex(x = url, pattern = "/$")) {
        url <- paste0(url, "/") # nocov
    }
    assert(isAnExistingURL(url))
    x <- RCurl::getURL(url = url, dirlistonly = TRUE)
    x <- strsplit(x, split = "\n")[[1L]]
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
    x <- sort(x)
    x
}
