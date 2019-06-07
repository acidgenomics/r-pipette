#' Load data as name
#'
#' @note This function is intended for interactive use and interprets object
#'   names using non-standard evaluation.
#'
#' @export
#'
#' @inheritParams loadData
#' @param ... Key value pairs, defining the name mappings. For example,
#'   `newName1` = `oldName1`, `newName2` = `oldName2`. Note that these
#'   arguments are interpreted using non-standard evaluation, and *should not
#'   be quoted*.
#'
#' @return Invisible named `character`. File paths.
#'
#' @examples
#' dir <- system.file("extdata", package = "brio")
#' loadDataAsName(renamed = example, dir = dir)
#' class(renamed)
# Last modified 2019-06-07.
loadDataAsName <- function(
    ...,
    dir,
    envir = globalenv(),
    overwrite
) {
    assert(
        is.environment(envir),
        isFlag(overwrite)
    )

    # Map the dot input to files.
    dots <- dots(..., character = TRUE)
    assert(hasNames(dots))
    files <- .listData(names = dots, dir = dir)
    names(files) <- names(dots)

    # Check to see if any of the new names already exist in environment.
    names <- names(dots)
    if (
        !isTRUE(overwrite) &&
        !isTRUE(allAreNonExisting(names, envir = envir, inherits = FALSE))
    ) {
        .loadExistsError(names)
    }

    # Note that we can skip safe loading here because we have already checked
    # for existing names in environment outside of the loop call.
    if (any(grepl("\\.rds$", files))) {
        # R data serialized: assign directly.
        invisible(mapply(
            name = names(files),
            file = files,
            FUN = function(name, file, envir) {
                data <- readRDS(file)
                assign(x = name, value = data, envir = envir)
            },
            MoreArgs = list(envir = envir)
        ))
    } else {
        # R data: use safe loading.
        safe <- new.env()
        invisible(mapply(
            FUN = .loadRDA,
            file = files,
            MoreArgs = list(
                envir = safe,
                # Note that we're checking for overwrite above already.
                overwrite = FALSE
            )
        ))
        assert(areSetEqual(dots, ls(safe)))

        # Now assign to the desired object names.
        invisible(mapply(
            FUN = function(from, to, safe, envir) {
                assign(
                    x = to,
                    value = get(from, envir = safe, inherits = FALSE),
                    envir = envir
                )
            },
            from = dots,
            to = names(dots),
            MoreArgs = list(safe = safe, envir = envir),
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
        ))
    }

    invisible(files)
}

formals(loadDataAsName)[["dir"]] <- formalsList[["load.dir"]]
formals(loadDataAsName)[["overwrite"]] <- formalsList[["overwrite"]]
