#' Load data
#'
#' Load R data files from a directory using symbols rather than complete file
#' paths. Supports `RDS`, `RDA`, and `RDATA` file extensions.
#'
#' `loadData()` is opinionated about the format of R data files it will accept.
#' `save()` allows for the saving of multiple objects into a single R data file.
#' This can later result in unexpected accidental replacement of an existing
#' object in the current environment. Since an R data file internally stores the
#' name of an object, if the file is later renamed the object name will no
#' longer match.
#'
#' To avoid any accidental replacements, `loadData()` will only load R data
#' files that contain a single object, and the internal object name must match
#' the file name exactly. Additionally, `loadData()` will intentionally error if
#' an object with the same name already exists in the destination `environment`.
#'
#' @export
#' @note This function is desired for interactive use and interprets object
#'   names using non-standard evaluation.
#' @note Updated 2021-08-24.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Object names.
#'   Note that these arguments are interpreted as symbols using non-standard
#'   evaluation for convenience during interactive use, and *must not be
#'   quoted*.
#' @param list `character`.
#'   A character vector containing the names of objects to be loaded.
#'
#' @return Invisible `character`.
#' File paths.
#'
#' @seealso
#' - `load()`.
#' - `readRDS()`.
#'
#' @examples
#' dir <- system.file("extdata", package = "pipette")
#'
#' ## Interactive mode ====
#' ## Note that this method uses non-standard evaluation.
#' loadData(example, dir = dir)
#'
#' ## Clean up.
#' rm(example, inherits = TRUE)
#'
#' ## List mode ====
#' ## Note that this method uses standard evaluation.
#' ## Use this approach inside of functions.
#' list <- "example"
#' loadData(list = list, dir = dir)
#'
#' ## Clean up.
#' rm(example, inherits = TRUE)
loadData <- function(
    ...,
    dir,
    envir = globalenv(),
    list = NULL,
    overwrite
) {
    assert(
        is.environment(envir),
        isCharacter(list, nullOK = TRUE),
        isFlag(overwrite)
    )
    if (isCharacter(list)) {
        names <- list
        rm(list)
        ## By default, assume user has passed in actual file paths.
        ## Otherwise, behave like NSE method, and attempt to add `dir`.
        if (isTRUE(allAreFiles(names))) {
            files <- realpath(names)  # nocov
        } else {
            files <- .listData(names = names, dir = dir)
        }
    } else {
        names <- dots(..., character = TRUE)
        files <- .listData(names = names, dir = dir)
    }
    assert(allAreFiles(files))
    if (all(grepl(
        pattern = "\\.rds$",
        x = files,
        ignore.case = TRUE
    ))) {
        fun <- .loadRDS
    } else if (all(grepl(
        pattern = "\\.rd[a|ata]$",
        x = files,
        ignore.case = TRUE
    ))) {
        fun <- .loadRDA
    } else {
        abort(sprintf(
            fmt = paste(
                "File extension error: %s.",
                "Don't mix %s files in a directory.",
                sep = "\n"
            ),
            toInlineString(basename(files), n = 5L),
            toInlineString(c("RDS", "RDA", "RDATA"))
        ))
    }
    lapply(
        X = files,
        FUN = fun,
        envir = envir,
        overwrite = overwrite
    )
    assert(allAreExisting(names, envir = envir, inherits = FALSE))
    invisible(files)
}

formals(loadData)[c("dir", "overwrite")] <-
    formalsList[c("load.dir", "overwrite")]



## Updated 2021-08-24.
.listData <- function(names, dir) {
    assert(isCharacter(names))
    dir <- realpath(dir)
    files <- vapply(
        X = names,
        FUN = function(name) {
            files <- sort(list.files(
                path = dir,
                pattern = paste0("^", name, .rdataExtPattern),
                full.names = TRUE,
                ignore.case = TRUE
            ))
            if (!hasLength(files)) {
                abort(sprintf(
                    "{.file %s} is missing in %{.path %s}.\n%s",
                    name, dir, .rdataLoadError
                ))
            } else if (length(files) > 1L) {
                abort(sprintf(
                    "{.file %s} is not unique at {.path %s}.\n%s",
                    name, dir, .rdataLoadError
                ))
            }
            files
        },
        FUN.VALUE = character(1L),
        USE.NAMES = TRUE
    )
    alert(sprintf(
        "Loading %s from {.path %s}.",
        toInlineString(basename(files), n = 10L, class = "file"),
        dir
    ))
    files
}



## Updated 2021-08-24.
.loadExistsError <- function(name) {
    abort(sprintf(
        fmt = paste(
            "{.var %s} exists in environment.",
            "Set {.code %s} to disable this check.",
            sep = "\n"
        ),
        name, "overwrite = TRUE"
    ))
}



## Updated 2019-06-07.
.loadRDS <- function(file, envir, overwrite) {
    file <- realpath(file)
    assert(
        isAFile(file),
        ## Allowing RDS only here.
        grepl("\\.rds$", file, ignore.case = TRUE),
        is.environment(envir),
        isFlag(overwrite)
    )
    name <- basenameSansExt(file)
    data <- readRDS(file)
    ## Error if the object is already assigned in environment.
    if (
        isFALSE(overwrite) &&
        exists(x = name, envir = envir, inherits = FALSE)
    ) {
        .loadExistsError(name)
    }
    assign(x = name, value = data, envir = envir)
    assert(exists(x = name, envir = envir, inherits = FALSE))
    invisible(file)
}



## Last modified 2019-06-07.
.loadRDA <- function(file, name = NULL, envir, overwrite) {
    file <- realpath(file)
    assert(
        isAFile(file),
        ## Allowing RDA or RDATA here.
        grepl("\\.rd[a|ata]$", file, ignore.case = TRUE),
        isString(name, nullOK = TRUE),
        is.environment(envir),
        isFlag(overwrite)
    )
    if (is.null(name)) {
        name <- basenameSansExt(file)
    }
    ## Error if the object is already assigned in environment.
    if (
        isFALSE(overwrite) &&
        exists(x = name, envir = envir, inherits = FALSE)
    ) {
        .loadExistsError(name)
    }
    ## Loading into a temporary environment, so we can evaluate the integrity
    ## of the objects before assigning into the destination environment.
    tmpEnvir <- new.env()
    loaded <- load(file, envir = tmpEnvir)
    ## Ensure that the loaded name is identical to the file name.
    if (!isString(loaded)) {
        abort(sprintf(
            "{.file %s} contains multiple objects: %s.",
            basename(file),
            toInlineString(loaded, n = 5L)
        ))
    }
    if (!identical(name, loaded)) {
        abort(sprintf(
            fmt = paste(
                "{.file %s} file has been renamed.",
                "The object name inside the file doesn't match.",
                "  - expected: {.var %s}",
                "  - actual:   {.var %s}",
                "Avoid renaming R data files.",
                "This can lead to accidental object replacement.",
                sep = "\n"
            ),
            basename(file), name, loaded
        ))
    }
    assert(identical(name, loaded))
    ## Now we're ready to assign into the target environment.
    assign(
        x = name,
        value = get(name, envir = tmpEnvir, inherits = FALSE),
        envir = envir
    )
    ## Ensure that assignment worked.
    assert(exists(x = name, envir = envir, inherits = FALSE))
    file
}
