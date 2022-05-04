#' Get remote URL directory listing
#'
#' @export
#' @note Best served using FTP instead of HTTP.
#' @note Updated 2022-05-04.
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
        requireNamespaces("RCurl"),
        isAURL(url),
        isString(pattern, nullOK = TRUE)
    )
    if (!grepl(pattern = "/$", x = url)) {
        url <- paste0(url, "/") # nocov
    }
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
