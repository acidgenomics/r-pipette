#' Save data
#'
#' Wrapper for `save()` supporting quick, interactive saving of object names
#' passed as symbols.
#'
#' This function always saves each object into a separate file rather than
#' combining multiple objects into a single file.
#'
#' @note This function is desired for interactive use and interprets object
#' names using non-standard evaluation. It will **overwrite** existing files
#' on disk, following the same conventions as `save()`.
#'
#' @export
#' @note Updated 2022-05-31.
#'
#' @inheritParams loadData
#' @inheritParams base::save
#' @inheritParams AcidRoxygen::params
#'
#' @param ext `character(1)`.
#' Output file format extension.
#'
#' Supported arguments:
#' - `"rds"`: R data serialized (RDS).
#' - `"rda"`: R data (RDA).
#'
#' RDS is preferred when saving single objects per file, which is always the
#' convention of `saveData()`, regardless of the extension used.
#'
#' @param list `character`.
#' A character vector containing the names of objects to be saved.
#' Note that this approach differs from `save()` in that the objects are saved
#' individually to disk, instead of inside a single R data file. Requires
#' objects to be defined in environment specified by `envir`. argument.
#'
#' @seealso
#' - `save()`.
#' - `saveRDS()`.
#'
#' @return Invisible named `character`.
#' File paths.
#'
#' @examples
#' dir <- file.path(tempdir(), "saveData")
#'
#' ## Interactive mode ====
#' ## Note that this method uses non-standard evaluation.
#' a <- 1
#' b <- 2
#' saveData(a, b, dir = dir)
#' sort(list.files(dir))
#'
#' ## Clean up.
#' unlink(normalizePath(dir), recursive = TRUE)
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
#' unlink(normalizePath(dir), recursive = TRUE)
saveData <-
    function(...,
             dir = getOption(
                 x = "acid.save.dir",
                 default = getwd()
             ),
             ext = getOption(
                 x = "acid.save.ext",
                 default = "rds"
             ),
             overwrite = getOption(
                 x = "acid.overwrite",
                 default = TRUE
             ),
             compress = getOption(
                 x = "acid.save.compress",
                 default = TRUE
             ),
             list = NULL,
             envir = parent.frame()) {
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
            objects <- objects[!file.exists(files)] # nocov
        }
        switch(
            EXPR = ext,
            "rds" = {
                Map(
                    f = saveRDS,
                    object = objects,
                    file = files,
                    MoreArgs = list(
                        "compress" = compress
                    )
                )
            },
            {
                Map(
                    f = save,
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
