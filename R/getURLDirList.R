#' Get remote URL directory listing
#'
#' @export
#' @note Best served using FTP instead of HTTP.
#' @note Updated 2019-10-18.
#'
#' @inheritParams acidroxygen::params
#'
#' @return `character`.
#'   Simple directory contents return, including both files and subdirectories.
#'
#' @examples
#' if (hasInternet()) {
#'     url <- "ftp://ftp.ensembl.org/pub/"
#'     x <- getURLDirList(url, pattern = "^release-")
#'     tail(x)
#' }
getURLDirList <- function(url, pattern = NULL) {
    assert(
        isAURL(url),
        isString(pattern, nullOK = TRUE)
    )
    if (!isTRUE(grepl("/$", url))) {
        stop("URL does not contain a trailing slash.")
    }
    x <- getURL(url = url, dirlistonly = TRUE)
    x <- unlist(strsplit(x, split = "\n"))
    if (isString(pattern)) {
        keep <- grepl(pattern = pattern, x = x)
        x <- x[keep]
    }
    x
}
