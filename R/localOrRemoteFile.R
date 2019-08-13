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
#' @note Updated 2019-07-19.
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
#' ## Local
#' file <- system.file("extdata/example.csv", package = "brio")
#' x <- localOrRemoteFile(file)
#' basename(x)
#'
#' ## Remote
#' file <- paste(brioTestsURL, "hgnc.txt.gz", sep = "/")
#' x <- localOrRemoteFile(file)
#' basename(x)
localOrRemoteFile <- function(file) {
    assert(isCharacter(file))
    if (!all(grepl(pattern = extPattern, x = file))) {
        stop(sprintf("'%s' does not end with file extension.", deparse(file)))
    }
    file <- mapply(
        file = file,
        FUN = function(file) {
            if (!isTRUE(isAURL(file))) {
                return(file)
            }
            ## Remote file mode.
            assert(hasInternet())
            ## Note that for `.gtf.gz` we want to return only `.gz` here.
            ## This behavor differs from matching using `extPattern` global.
            ext <- str_match(
                string = basename(file),
                pattern = "\\.([a-zA-Z0-9]+)$"
            )
            ext <- na.omit(ext[1L, 2L])
            assert(hasLength(ext))
            ## Write mode for binary files. Applies to Windows.
            ## https://github.com/tidyverse/readxl/issues/374
            binary <- c(
                "bz2",
                "gz",
                "rda",
                "rds",
                "xls",
                "xlsx",
                "xz",
                "zip"
            )
            if (ext %in% binary) {
                ## Write binary.
                mode <- "wb"
            } else {
                ## Write (default).
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



## Auto decompress, if necessary. Note that `data.table::fread()` still doesn't
## natively support compressed files. R on Windows can run into `tempdir()`
## write permission issues, unless R is running as administrator. Ensure that
## decompressed is removed manually before attempting to overwrite, otherwise
## this step can error out.
.autoDecompress <- function(file) {
    file <- realpath(file)
    vapply(
        X = file,
        FUN = function(file) {
            if (!grepl(compressExtPattern, file)) {
                return(file)
            }
            message(sprintf(
                "Decompressing '%s' in '%s'",
                basename(file), "tempdir()"
            ))

            ## Get the compression extension and decompressed file basename.
            match <- str_match(
                string = basename(file),
                pattern = compressExtPattern
            )
            assert(is.matrix(match), nrow(match) == 1L)
            match <- match[1L, , drop = TRUE]
            compressExt <- toupper(match[[2L]])

            ## Attempt to force removal of an existing decompressed file on
            ## Windows, which can error out on some machines. Fail with a clear
            ## error message if and when this occurs.
            if (identical(.Platform[["OS.type"]], "windows")) {
                ## nocov start
                decompressedFile <- sub(
                    pattern = compressExtPattern,
                    replacement = "",
                    x = basename(file)
                )
                .removeTempFile(decompressedFile)
                ## nocov end
            }

            if (compressExt %in% c("BZ2", "GZ", "XZ")) {
                ## Using the R.utils package to handle BZ2, GZ, XZ.
                if (compressExt == "BZ2") {
                    fun <- bzfile
                } else if (compressExt == "GZ") {
                    fun <- gzfile
                } else if (compressExt == "XZ") {
                    fun <- xzfile
                }
                file <- decompressFile(
                    filename = file,
                    ext = compressExt,
                    FUN = fun,
                    temporary = TRUE,
                    skip = FALSE,
                    overwrite = TRUE,
                    remove = FALSE
                )
            } else if (compressExt == "ZIP") {
                ## Using the utils package to handle ZIP.
                file <- unzip(
                    zipfile = file,
                    overwrite = TRUE,
                    exdir = tempdir()
                )
                ## Ensure we're returning a string.
                file <- file[[1L]]
            }
            file
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
}



## Fix attempt for Windows R erroring out on failure to overwrite tempfile.
## This can happen for some non-admin user accounts, which is annoying.
## https://support.rstudio.com/hc/en-us/community/posts/115007456107
## nocov start
.removeTempFile <- function(file) {
    file <- file.path(tempdir(), file)
    if (file.exists(file)) {
        file.remove(file)
    }
    if (file.exists(file)) {
        unlink(file, force = TRUE)
    }
    if (file.exists(file)) {
        stop(
            "Failed to remove temporary file.\n",
            "This is a known issue with R on Windows.\n",
            "Consider these alternatives:\n",
            "  - Set TMPDIR to an alternate location in '.Renviron' file.\n",
            "  - Run R as Administrator.\n",
            "  - Switch to macOS or Linux."
        )
    }
    invisible()
}
## nocov end
