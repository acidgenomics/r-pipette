#' Save data
#'
#' Wrapper for `save()` supporting quick, interactive saving of object names
#' passed as symbols.
#'
#' This function always saves each object into a separate file rather than
#' combining multiple objects into a single file.
#'
#' @note This function is desired for interactive use and interprets object
#'   names using non-standard evaluation. It will **overwrite** existing files
#'   on disk, following the same conventions as `save()`.
#'
#' @export
#' @note Updated 2020-08-11.
#'
#' @inheritParams loadData
#' @inheritParams base::save
#' @inheritParams AcidRoxygen::params
#' @param ext `character(1)`.
#'   Output file format extension.
#'
#'   Supported arguments:
#'   - `"rds"`: R data serialized (RDS).
#'   - `"rda"`: R data (RDA).
#'
#'   RDS is preferred when saving single objects per file, which is always the
#'   convention of `saveData()`, regardless of the extension used.
#' @param list `character`.
#'   A character vector containing the names of objects to be saved.
#'   Note that this approach differs from `save()` in that the objects are saved
#'   individually to disk, instead of inside a single R data file. Requires
#'   objects to be defined in environment specified by `envir`. argument.
#'
#' @seealso
#' - `save()`.
#' - `saveRDS()`.
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
        assert(
            isCharacter(list),
            is.environment(envir)
        )
        objects <- mget(x = list, envir = envir, inherits = FALSE)
        names(objects) <- list
        rm(list)
    } else {
        objects <- list(...)
        names(objects) <- dots(..., character = TRUE)
    }
    dir <- initDir(dir)
    ext <- match.arg(arg = ext, choices = c("rds", "rda"))
    files <- file.path(dir, paste(names(objects), ext, sep = "."))
    names(files) <- names(objects)
    alert(sprintf(
        "Saving %s to {.path %s}.",
        toInlineString(basename(files), n = 10L, class = "file"),
        dir
    ))
    if (identical(overwrite, FALSE) && any(file.exists(files))) {
        skip <- files[file.exists(files)]
        alertWarning(sprintf(
            "Skipped %s.",
            toInlineString(basename(skip), n = 10L, class = "file")
        ))
        files <- files[!file.exists(files)]
        if (!hasLength(files)) {
            alertWarning("No files were saved.")
            return(invisible(NULL))
        }
        objects <- objects[!file.exists(files)]  # nocov
    }
    switch(
        EXPR = ext,
        "rds" = {
            mapply(
                FUN = saveRDS,
                object = objects,
                file = files,
                MoreArgs = list(
                    "compress" = compress
                )
            )
        },
        {
            mapply(
                FUN = save,
                list = names(files),
                file = files,
                MoreArgs = list(
                    "envir" = parent.frame(),
                    "compress" = compress
                )
            )
        }
    )
    invisible(files)
}

formals(saveData)[
    c("compress", "dir", "ext", "overwrite")] <-
    formalsList[c("save.compress", "save.dir", "save.ext", "overwrite")]



#' Assign and save data
#'
#' Assigns a new object by name to the current working environment then saves
#' the newly assigned object, specified by the `"dir"` argument.
#'
#' @export
#' @note This function attempts to follow the same order as `assign()`.
#' @note Updated 2019-10-12.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams saveData
#' @param name `character(1)`.
#'   Desired variable name.
#' @param envir `environment`.
#'   Environment to use for assignment.
#'   Defaults to `parent.frame()`, the calling environment.
#'
#' @return Invisible named `character(1)`.
#' File path.
#'
#' @examples
#' x <- 1L
#' assignAndSaveData(
#'     name = "example",
#'     object = x,
#'     dir = ".",
#'     ext = "rds"
#' )
#' exists("example", inherits = FALSE)
#' file.exists("example.rds")
#'
#' ## Clean up.
#' rm(example)
#' unlink("example.rds")
assignAndSaveData <- function(name, object, envir = parent.frame()) {
    assert(
        isString(name),
        !is.null(object),
        formalCompress(compress),
        is.environment(envir)
    )
    ## Create destination directory automatically.
    dir <- initDir(dir)
    ## Assign data.
    assign(x = name, value = object, envir = envir)
    assign(x = name, value = object)
    ## Save data.
    args <- list(
        as.name(name),
        dir = dir,
        ext = ext,
        overwrite = overwrite,
        compress = compress
    )
    ## Return file path.
    file <- do.call(what = saveData, args = args)
    invisible(file)
}

f1 <- formals(assignAndSaveData)
f2 <- formals(saveData)
f2 <- f2[setdiff(names(f2), c(names(f1), "list", "..."))]
f <- c(f1, f2)
formals(assignAndSaveData) <- f
