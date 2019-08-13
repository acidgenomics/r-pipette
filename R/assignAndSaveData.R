#' Assign and save data
#'
#' Assigns a new object by name to the current working environment then saves
#' the newly assigned object, specified by the `"dir"` argument.
#'
#' @note This function attempts to follow the same order as
#'   [`assign()`][base::assign].
#' @note Updated 2019-07-19.
#' @include saveData.R
#' @export
#'
#' @inheritParams acidroxygen::params
#' @inheritParams saveData
#' @param name `character(1)`.
#'   Desired variable name.
#' @param envir `environment`.
#'   Environment to use for assignment.
#'   Defaults to [parent.frame()], the calling environment.
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
    file <- do.call(what = saveData, args = args)

    invisible(file)
}

f1 <- formals(assignAndSaveData)
f2 <- formals(saveData)
f2 <- f2[setdiff(names(f2), c(names(f1), "list", "..."))]
f <- c(f1, f2)
formals(assignAndSaveData) <- f
