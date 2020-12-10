#' Download a file from the Internet
#'
#' @details
#' Unlike [utils::download.file()], intentionally errors on any download
#' failure. Automatically sets a long time out internally via
#' `options(timeout = 9999L)`, to avoid download issues related to `timeout`
#' configuration in `Rprofile`.
#'
#' @param url `character(1)`.
#'   URL.
#' @param destfile `character(1)`.
#'   Destination file.
#' @param ... Passthrough arguments to [utils::download.file()].
#'
#' @return `character(1)`.
#'   Destination file path.
#'   Note that this differs from `download.file`, which returns a status code
#'   (e.g. `0` for success) instead.
#'
#' @examples
#' url <- "https://bioconductor.org/bioc-version"
#' destfile <- "bioc-version.txt"
#' out <- download(url = url, destfile = destfile)
#' print(out)
#' file.remove(out)
download <-
    function(url, destfile, ...) {
        assert(
            isAURL(url),
            isString(destfile)
        )
        timeout <- getOption("timeout")
        if (is.numeric(timeout)) {
            options("timeout" = 99999L)
        }
        status <- download.file(url = url, destfile = destfile, ...)
        if (!identical(status, 0L)) {
            stop(sprintf(
                "Failed to download '%s' to '%s' successfully.",
                url, destfile
            ))
        }
        if (is.numeric(timeout)) {
            options("timeout" = timeout)
        }
        invisible(destfile)
    }
