#' Write counts
#'
#' Supports both bulk and single-cell RNA-seq count matrices. Bulk RNA-seq
#' counts are written to disk as comma separated values ("`.CSV`"). Single-cell
#' RNA-seq counts are written to disk in MatrixMarket format ("`.MTX`"), along
#' with the sample barcodes ("`.COLNAMES`"), and gene identifiers
#' ("`.ROWNAMES`").
#'
#' Automatic gzip compression is offered as a user-defined option. This setting
#' is enabled by default to save disk space. Note that the [readr][] package,
#' built into [RStudio][], now natively supports compressed files.
#'
#' [readr]: http://readr.tidyverse.org/
#' [RStudio]: https://www.rstudio.com/
#'
#' @export
#' @inheritParams dots
#' @inheritParams saveData
#'
#' @param ... Symbols.
#'   Unquoted object names containing count matrices.
#' @param dir `character(1)`.
#'   Output directory.
#' @param compress `logical(1)`.
#'   Compress the files using gzip.
#'
#' @return Invisible `list`.
#' File paths.
#'
#' @note This function is desired for interactive use and interprets object
#' names using non-standard evaluation.
#'
#' @examples
#' counts <- matrix(data = seq_len(100L), nrow = 10)
#' writeCounts(counts, dir = "example")
#'
#' ## Clean up.
#' unlink("example", recursive = TRUE)

# Updated 2019-07-19.
writeCounts <- function(..., dir, compress) {
    names <- dots(..., character = TRUE)
    data <- list(...)
    dir <- initDir(dir)
    assert(isFlag(compress))

    ## Iterate across the dot objects and write to disk.
    message(paste0("Writing ", toString(names), " to ", dir, "."))

    files <- mapply(
        name = names,
        object = data,
        FUN = function(name, object) {
            if (is.matrix(object)) {
                if (isTRUE(compress)) {
                    format <- "csv.gz"
                } else {
                    format <- "csv"
                }
            } else if (is(object, "sparseMatrix")) {
                if (isTRUE(compress)) {
                    format <- "mtx.gz"
                } else {
                    format <- "mtx"
                }
            } else {
                stop(paste(name, "is not a matrix."))
            }
            file <- file.path(dir, paste0(name, ".", format))
            export(object = object, file = file)
        },
        SIMPLIFY = FALSE,
        USE.NAMES = TRUE
    )

    invisible(files)
}

formals(writeCounts)[["compress"]] <- formalsList[["export.compress"]]
formals(writeCounts)[["dir"]] <- formalsList[["export.dir"]]
