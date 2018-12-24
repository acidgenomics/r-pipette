#' Save data
#'
#' Wrapper for [`save()`][base::save] supporting quick, interactive saving of
#' object names passed as symbols.
#'
#' This function always saves each object into a separate file rather than
#' combining multiple objects into a single file.
#'
#' @note This function is desired for interactive use and interprets object
#'   names using non-standard evaluation. It will **overwrite** existing files
#'   on disk, following the same conventions as [`save()`][base::save].
#'
#' @export
#' @include globals.R
#' @inheritParams loadData
#' @inheritParams base::save
#'
#' @param overwrite `logical(1)`.
#'   Overwrite existing file.
#' @param ext `character(1)`.
#'   R data serialized (RDS; "`rds`") or R data ("`rda`", "`RData`"). RDS is
#'   preferred when saving single objects per file, which is always the
#'   convention of `saveData`, regardless of the extension used.
#'
#' @seealso
#' - [`save()`][base::save]
#' - [`saveRDS()`][base::saveRDS].
#'
#' @return Invisible named `character`.
#' File paths.
#'
#' @examples
#' x <- 1
#' saveData(x, dir = "example")
#' list.files("example")
#'
#' ## Clean up.
#' unlink("example", recursive = TRUE)
saveData <- function(..., dir, ext, overwrite, compress) {
    objects <- list(...)
    names(objects) <- dots(..., character = TRUE)
    dir <- initDir(dir)
    ext <- match.arg(arg = ext, choices = c("rds", "rda", "RData"))
    assert(
        isFlag(overwrite),
        formalCompress(compress)
    )

    files <- file.path(dir, paste(names(objects), ext, sep = "."))
    names(files) <- names(objects)

    message(paste("Saving", toString(basename(files)), "to", dir))

    # If `overwrite = FALSE`, inform the user which files were skipped
    if (identical(overwrite, FALSE) && any(file.exists(files))) {
        skip <- files[file.exists(files)]
        warning(paste0(
            "Skipped ", toString(basename(skip)), "."
        ), call. = FALSE)
        files <- files[!file.exists(files)]
        if (length(files) == 0L) {
            warning("No files were saved.")
            return(invisible())
        }
        objects <- objects[!file.exists(files)]  # nocov
    }

    # Determine which save function to use.
    if (ext == "rds") {
        mapply(
            FUN = saveRDS,
            object = objects,
            file = files,
            MoreArgs = list(compress = compress)
        )
    } else {
        mapply(
            FUN = save,
            list = names(files),
            file = files,
            MoreArgs = list(
                envir = parent.frame(),
                compress = compress
            )
        )
    }

    invisible(files)
}

formals(saveData)[["compress"]] <- formalsList[["save.compress"]]
formals(saveData)[["dir"]] <- formalsList[["save.dir"]]
formals(saveData)[["ext"]] <- formalsList[["save.ext"]]
formals(saveData)[["overwrite"]] <- formalsList[["save.overwrite"]]
