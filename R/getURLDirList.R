## nocov start
## This check can fail on Travis CI due to remove server dependency.



#' Get remote URL directory listing
#'
#' @export
#' @note Best served using FTP instead of HTTP.
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `character`.
#'   Simple directory contents return, including both files and subdirectories.
#'
#' @examples
#' url <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
#' if (
#'     !isTRUE(nzchar(Sys.getenv("CI"))) &&
#'     goalie::hasInternet(url)
#' ) {
#'     x <- getURLDirList(url)
#'     tail(x)
#' }
getURLDirList <- function(url, pattern = NULL) {
    requireNamespaces("RCurl")
    assert(
        isAURL(url),
        isString(pattern, nullOK = TRUE)
    )
    if (!isTRUE(grepl("/$", url))) {
        url <- paste0(url, "/")  # nocov
    }
    x <- RCurl::getURL(url = url, dirlistonly = TRUE)
    x <- unlist(strsplit(x, split = "\n"))
    if (isString(pattern)) {
        keep <- grepl(pattern = pattern, x = x)
        x <- x[keep]
    }
    x <- sort(x)
    x
}



## nocov end
