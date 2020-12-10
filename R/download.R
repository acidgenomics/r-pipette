#' Download a file from the Internet
#'
#' @export
#' @note Updated 2020-12-10.
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
    function(
        url,
        destfile,
        quiet = FALSE,
        ...
    ) {
        assert(
            isAURL(url),
            isString(destfile),
            isFlag(quiet)
        )
        destfile <- normalizePath(destfile, mustWork = FALSE)
        timeout <- getOption("timeout")
        if (is.numeric(timeout)) {
            options("timeout" = 99999L)
        }
        if (isFALSE(quiet)) {
            cli_alert(sprintf(
                "Downloading {.url %s} to {.file %s}.",
                url, destfile
            ))
        }
        status <- download.file(
            url = url,
            destfile = destfile,
            quiet = quiet,
            ...
        )
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
