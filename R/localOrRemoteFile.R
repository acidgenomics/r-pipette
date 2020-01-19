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
#' @note Updated 2020-01-19.
#'
#' @inheritParams acidroxygen::params
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
#' file <- system.file("extdata/example.csv", package = "pipette")
#' x <- localOrRemoteFile(file)
#' basename(x)
#'
#' ## Remote
#' file <- acidbase::pasteURL(
#'     pipetteTestsURL,
#'     "hgnc.txt.gz",
#'     protocol = "none"
#' )
#' x <- localOrRemoteFile(file)
#' basename(x)
localOrRemoteFile <- function(file, quiet) {
    assert(
        isCharacter(file),
        isFlag(quiet)
    )
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
            ## Write mode for binary files.
            ## Note that files without extension will use default.
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
            if (isSubset(ext, binary)) {
                ## Write binary.
                mode <- "wb"
            } else {
                ## Write (default).
                mode <- "w"
            }
            destfile <- file.path(tempdir(), basename(file))
            download.file(
                url = file,
                destfile = destfile,
                quiet = quiet,
                mode = mode
            )
            destfile
        },
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
    .autoDecompress(file = file, quiet = quiet)
}

formals(localOrRemoteFile)[["quiet"]] <- formalsList[["quiet"]]



## Auto decompress, if necessary. Note that `data.table::fread()` still doesn't
## natively support compressed files. R on Windows can run into `tempdir()`
## write permission issues, unless R is running as administrator. Ensure that
## decompressed is removed manually before attempting to overwrite, otherwise
## this step can error out.
.autoDecompress <- function(file, quiet) {
    assert(isFlag(quiet))
    file <- realpath(file)
    vapply(
        X = file,
        FUN = function(file) {
            if (!grepl(compressExtPattern, file)) {
                return(file)
            }
            if (!isTRUE(quiet)) {
                cli_alert(sprintf(
                    "Decompressing {.file %s} in {.path %s}.",
                    basename(file), "tempdir()"
                ))
            }
            ## Get the compression extension and decompressed file basename.
            match <- str_match(
                string = basename(file),
                pattern = compressExtPattern
            )
            assert(is.matrix(match), nrow(match) == 1L)
            match <- match[1L, , drop = TRUE]
            compressExt <- toupper(match[[2L]])
            if (compressExt %in% c("BZ2", "GZ", "XZ")) {
                ## Using the R.utils package to handle BZ2, GZ, XZ.
                if (compressExt == "BZ2") {
                    fun <- bzfile
                } else if (compressExt == "GZ") {
                    fun <- gzfile
                } else if (compressExt == "XZ") {
                    fun <- xzfile
                }
                ## FIXME Can we use base method instead of R.utils here?
                ## FIXME Switch to using gzfile, bzfile, xzfile internally.
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
