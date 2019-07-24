#' Load data
#'
#' Load R data files from a directory using symbols rather than complete file
#' paths. Supports `RDS`, `RDA`, and `RDATA` file extensions.
#'
#' [loadData()] is opinionated about the format of R data files it will accept.
#' [`save()`][base::save] allows for the saving of multiple objects into a
#' single R data file. This can later result in unexpected accidental
#' replacement of an existing object in the current environment. Since an R data
#' file internally stores the name of an object, if the file is later renamed
#' the object name will no longer match.
#'
#' To avoid any accidental replacements, [loadData()] will only load R data
#' files that contain a single object, and the internal object name must match
#' the file name exactly. Additionally, [loadData()] will intentionally error if
#' an object with the same name already exists in the destination `environment`.
#'
#' @note This function is desired for interactive use and interprets object
#'   names using non-standard evaluation.
#'
#' @export
#' @inheritParams params

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
#' - [`load()`][base::load].
#' - [`readRDS()`][base::readRDS].
#'
#' @examples
#' dir <- system.file("extdata", package = "brio")
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

## Updated 2019-06-07.
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
            files <- realpath(names)
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
        stop(paste0(
            "File extension error: ",
            toString(basename(files)), "\n",
            "Don't mix RDS, RDA, and/or RDATA files in a single directory."
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

formals(loadData)[["dir"]] <- formalsList[["load.dir"]]
formals(loadData)[["overwrite"]] <- formalsList[["overwrite"]]



.listData <- function(names, dir) {
    assert(isCharacter(names))
    dir <- realpath(dir)
    files <- vapply(
        X = names,
        FUN = function(name) {
            files <- sort(list.files(
                path = dir,
                pattern = paste0("^", name, rdataExtPattern),
                full.names = TRUE,
                ignore.case = TRUE
            ))
            ## Add error checking here.
            if (length(files) == 0L) {
                stop(paste0(
                    deparse(name), " is missing.\n",
                    "dir: ", dir, "\n",
                    rdataLoadError
                ))
            } else if (length(files) > 1L) {
                stop(paste0(
                    deparse(name), " is not unique on disk.\n",
                    "dir: ", dir, "\n",
                    rdataLoadError
                ))
            }
            files
        },
        FUN.VALUE = character(1L),
        USE.NAMES = TRUE
    )
    message(paste("Loading", toString(basename(files)), "from", dir))
    files
}



.loadExistsError <- function(name) {
    stop(paste0(
        deparse(name), " exists in environment.\n",
        "Set `overwrite = TRUE` to disable this check."
    ))
}



## Last modified 2019-06-07.
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
        !isTRUE(overwrite) &&
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
        !isTRUE(overwrite) &&
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
        stop(paste0(
            basename(file),
            " contains multiple objects: ",
            toString(loaded)
        ))
    }
    if (!identical(name, loaded)) {
        stop(paste0(
            basename(file), " has been renamed.\n",
            "The object name inside the file doesn't match.\n",
            "  expected: ", name, "\n",
            "    actual: ", loaded, "\n",
            "Avoid renaming R data files. ",
            "This can lead to accidental replacement."
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
