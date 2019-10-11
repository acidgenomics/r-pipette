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
#' @note Updated 2019-10-11.
#' @include globals.R
#' @export
#'
#' @inheritParams loadData
#' @inheritParams base::save
#' @inheritParams acidroxygen::params
#' @param ext `character(1)`.
#'   R data serialized (`RDS`) or R data (`RDA`, `RDATA`). RDS is
#'   preferred when saving single objects per file, which is always the
#'   convention of [saveData()], regardless of the extension used.
#' @param list `character`.
#'   A character vector containing the names of objects to be saved.
#'   Note that this approach differs from [`save()`][base::save] in that the
#'   objects are saved individually to disk, instead of inside a single R data
#'   file. Requires objects to be defined in environment specified by `envir`.
#'   argument.
#'
#' @seealso
#' - [`save()`][base::save]
#' - [`saveRDS()`][base::saveRDS].
#'
#' @return Invisible named `character`.
#' File paths.
#'
#' @examples
#' dir <- "example"
#'
#' ## Interactive mode ====
#' ## Note that this method uses non-standard evaluation.
#' a <- 1
#' b <- 2
#' saveData(a, b, dir = dir)
#' sort(list.files(dir))
#'
#' ## Clean up.
#' unlink(dir, recursive = TRUE)
#'
#' ## List mode ====
#' ## Note that this method uses standard evaluation.
#' ## Use this approach inside of functions.
#' a <- 1
#' b <- 2
#' list <- c("a", "b")
#' saveData(list = list, dir = dir)
#' sort(list.files(dir))
#'
#' ## Clean up.
#' unlink(dir, recursive = TRUE)
saveData <- function(
    ...,
    dir,
    ext,
    overwrite,
    compress,
    list = NULL,
    envir = parent.frame()
) {
    assert(
        isFlag(overwrite),
        formalCompress(compress)
    )
    if (!is.null(list)) {
        ## Character vector list mode (similar to `save()`).
        assert(
            isCharacter(list),
            is.environment(envir)
        )
        objects <- mget(x = list, envir = envir, inherits = FALSE)
        names(objects) <- list
        rm(list)
    } else {
        ## Non-standard evaluation mode (default).
        objects <- list(...)
        names(objects) <- dots(..., character = TRUE)
    }
    dir <- initDir(dir)
    ext <- match.arg(arg = ext, choices = c("rds", "rda"))
    files <- file.path(dir, paste(names(objects), ext, sep = "."))
    names(files) <- names(objects)
    message(sprintf(
        "Saving %s to '%s'.",
        toString(
            paste0("'", basename(files), "'"),
            width = 200L
        ),
        dir
    ))
    ## If `overwrite = FALSE`, inform the user which files were skipped.
    if (identical(overwrite, FALSE) && any(file.exists(files))) {
        skip <- files[file.exists(files)]
        warning(sprintf("Skipped %s.", toString(basename(skip))))
        files <- files[!file.exists(files)]
        if (length(files) == 0L) {
            warning("No files were saved.")
            return(invisible())
        }
        objects <- objects[!file.exists(files)]  # nocov
    }
    ## Determine which save function to use.
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
formals(saveData)[["overwrite"]] <- formalsList[["overwrite"]]
