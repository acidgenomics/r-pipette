#' Assign and save data
#'
#' Assigns a new object by name to the current working environment then saves
#' the newly assigned object, specified by the `"dir"` argument.
#'
#' @export
#' @note This function attempts to follow the same order as `assign()`.
#' @note Updated 2022-05-23.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams saveData
#'
#' @param name `character(1)`.
#' Desired variable name.
#'
#' @param envir `environment`.
#' Environment to use for assignment.
#' Defaults to `parent.frame()`, the calling environment.
#'
#' @return Invisible named `character(1)`.
#' File path.
#'
#' @examples
#' x <- 1L
#' assignAndSaveData(
#'     name = "example",
#'     object = x,
#'     dir = getwd(),
#'     ext = "rds"
#' )
#' exists("example", inherits = FALSE)
#' file.exists("example.rds")
#'
#' ## Clean up.
#' rm(example)
#' unlink("example.rds")
assignAndSaveData <-
    function(name,
             object,
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
             envir = parent.frame()) {
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
            "dir" = dir,
            "ext" = ext,
            "overwrite" = overwrite,
            "compress" = compress
        )
        ## Return file path.
        file <- do.call(what = saveData, args = args)
        invisible(file)
    }
