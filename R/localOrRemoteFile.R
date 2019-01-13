#' Dynamically handle a local or remote file path
#'
#' @section Vectorization:
#'
#' This function is vectorized and supports mixed local and remote paths. Remote
#' files are downloaded locally to a temporary directory.
#'
#' @section Compressed files:
#'
#' Compressed files will automatically be decompressed. Currently, these file
#' extensions are natively supported: `BZ2`, `GZ`, `XZ`, `ZIP`.
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
    file <- mapply(
        file = file,
        FUN = function(file) {
            if (!isTRUE(isAURL(file))) {
                return(file)
            }
            # Remote file mode.
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
        },
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )

    .autoDecompress(file)
}



# Auto decompress, if necessary.
# Note that `data.table::fread()` still doesn't natively support this.
.autoDecompress <- function(file) {
    file <- realpath(file)
    vapply(
        X = file,
        FUN = function(file) {
            if (!grepl(compressExtPattern, file)) {
                return(file)
            }
            message(paste("Decompressing", basename(file), "in tempdir()."))
            compressExt <-
                toupper(str_match(basename(file), compressExtPattern)[1L, 2L])
            if (compressExt %in% c("BZ2", "GZ", "XZ")) {
                # Using the R.utils package to handle BZ2, GZ, XZ.
                if (compressExt == "BZ2") {
                    FUN <- bzfile
                } else if (compressExt == "GZ") {
                    FUN <- gzfile
                } else if (compressExt == "XZ") {
                    FUN <- xzfile
                }
                file <- decompressFile(
                    filename = file,
                    ext = compressExt,
                    FUN = FUN,
                    temporary = TRUE,
                    skip = FALSE,
                    overwrite = TRUE,
                    remove = FALSE
                )
            } else if (compressExt == "ZIP") {
                # Using the utils package to handle ZIP.
                file <- unzip(
                    zipfile = file,
                    overwrite = TRUE,
                    exdir = tempdir()
                )
                # Ensure we're returning a string.
                file <- file[[1L]]
            }
            file
        },
        FUN.VALUE = character(1),
        USE.NAMES = FALSE
    )
}
