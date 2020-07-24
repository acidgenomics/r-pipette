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
#' extensions are natively supported: `BZ2`, `GZ`, `XZ`, and `ZIP`.
#'
#' @export
#' @note Updated 2020-07-24.
#'
#' @inheritParams acidroxygen::params
#' @param file `character(1)`.
#'   Local file paths or remote URLs.
#'
#' @return `character`.
#' Local file path(s). Stops on a missing file.
#'
#' @seealso
#' - [tempfile()].
#' - [tempdir()].
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
            tmpdir <- realpath(tempdir())
            destfile <- tempfile(
                pattern = "pipette-",
                tmpdir = tmpdir,
                fileext = paste0(".", fileExt(file))
            )
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
    realpath(.autoDecompress(file))
}

formals(localOrRemoteFile)[["quiet"]] <- formalsList[["quiet"]]



## Auto decompress, if necessary. Note that `data.table::fread()` still doesn't
## natively support compressed files. R on Windows can run into `tempdir()`
## write permission issues, unless R is running as administrator. Ensure that
## decompressed is removed manually before attempting to overwrite, otherwise
## this step can error out.
## Updated 2020-07-24.
.autoDecompress <- function(file) {
    vapply(
        X = realpath(file),
        FUN = function(file) {
            if (!grepl(compressExtPattern, file)) {
                return(file)
            }
            tmpdir <- realpath(tempdir())
            if (!isTRUE(grepl(pattern = tmpdir, x = file))) {
                tmpfile <- tempfile(
                    pattern = "pipette-",
                    tmpdir = tmpdir,
                    fileext = paste0(".", fileExt(file))
                )
                file.copy(from = file, to = tmpfile)
                file <- tmpfile
            }
            destfile <- decompress(
                file = file,
                remove = FALSE,
                overwrite = TRUE
            )
            assert(isString(destfile))
            destfile
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
}
