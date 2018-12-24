#' Dynamically handle a local or remote file path
#'
#' @note
#' This function is vectorized and supports mixed local and remote paths. Remote
#' files are downloaded locally to a temporary directory.
#'
#' @export
#'
#' @param file `character(1)`.
#'   Local file paths or remote URLs.
#'
#' @return `character`.
#' Local file path(s). Stops on a missing file.
#'
#' @seealso [tempdir()].
#'
#' @examples
#' file <- system.file("extdata/example.csv", package = "brio")
#' x <- localOrRemoteFile(file)
#' basename(x)
localOrRemoteFile <- function(file) {
    assert(isCharacter(file))
    if (!all(grepl(pattern = extPattern, x = file))) {
        stop(paste(deparse(file), "does not end with file extension."))
    }
    local <- mapply(
        file = file,
        FUN = function(file) {
            # Remote file mode.
            if (isTRUE(isAURL(file))) {
                assert(hasInternet())
                ext <- str_match(basename(file), extPattern)[1L, 2L:3L]
                ext <- na.omit(ext)
                ext <- paste(ext, collapse = "")
                assert(hasLength(ext))
                # Fix for binary files (typically on Windows).
                # https://github.com/tidyverse/readxl/issues/374
                binary <- c(
                    "bz2", "gz", "rda", "rds", "xls", "xlsx", "xz", "zip"
                )
                if (ext %in% binary) {
                    # Write binary.
                    mode <- "wb"
                } else {
                    # Write (default).
                    mode <- "w"
                }
                destfile <- file.path(tempdir(), basename(file))
                download.file(url = file, destfile = destfile, mode = mode)
                destfile
            } else {
                file
            }
        },
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
    realpath(local)
}
