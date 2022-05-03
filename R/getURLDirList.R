#' Get remote URL directory listing
#'
#' @export
#' @note Best served using FTP instead of HTTP.
#' @note Updated 2022-05-03.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `character`.
#' Simple directory contents return, including both files and subdirectories.
#'
#' @examples
#' url <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
#' if (goalie::hasInternet(url)) {
#'     x <- getURLDirList(url)
#'     tail(x)
#' }
getURLDirList <- function(url, pattern = NULL) {
    assert(
        isAURL(url),
        isString(pattern, nullOK = TRUE)
    )
    if (isFALSE(isMatchingRegex(x = url, pattern = "/$"))) {
        url <- paste0(url, "/") # nocov
    }
    x <- getURL(url = url, dirlistonly = TRUE)
    x <- strsplit(x, split = "\n")[[1L]]
    if (isString(pattern)) {
        keep <- isMatchingRegex(x = x, pattern = pattern)
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
