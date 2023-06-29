#' Load data as name
#'
#' @export
#' @note This function is intended for interactive use and interprets object
#' names using non-standard evaluation.
#' @note Updated 2023-06-29.
#'
#' @inheritParams loadData
#' @param ... Key value pairs, defining the name mappings. For example,
#' `newName1` = `oldName1`, `newName2` = `oldName2`. Note that these
#' arguments are interpreted using non-standard evaluation, and *should not
#' be quoted*.
#'
#' @return Invisible named `character`. File paths.
#'
#' @examples
#' dir <- system.file("extdata", package = "pipette")
#' loadDataAsName(renamed = example, dir = dir)
#' class(renamed)
loadDataAsName <-
    function(...,
             dir = getOption(
                 x = "acid.load.dir",
                 default = getwd()
             ),
             envir = globalenv(),
             overwrite = getOption(
                 x = "acid.overwrite",
                 default = TRUE
             )) {
        assert(
            isADir(dir),
            is.environment(envir),
            isFlag(overwrite)
        )
        ## Map the dot input to files.
        dots <- dots(..., character = TRUE)
        assert(hasNames(dots))
        files <- .listData(names = dots, dir = dir)
        names(files) <- names(dots)
        ## Check to see if any of the new names already exist in environment.
        names <- names(dots)
        if (
            isFALSE(overwrite) &&
                isFALSE(allAreNonExisting(
                    x = names,
                    envir = envir,
                    inherits = FALSE
                ))
        ) {
            .loadExistsError(names)
        }
        if (allAreMatchingRegex(
            x = tolower(basename(files)),
            pattern = "\\.rds$"
        )) {
            ## R data serialized: assign directly.
            invisible(Map(
                name = names(files),
                file = files,
                f = function(name, file, envir) {
                    data <- readRDS(file)
                    assign(x = name, value = data, envir = envir)
                },
                MoreArgs = list("envir" = envir)
            ))
        } else if (allAreMatchingRegex(
            x = tolower(basename(files)),
            pattern = "\\.rd[a|ata]$"
        )) {
            ## R data: use safe loading.
            safe <- new.env()
            invisible(Map(
                f = .loadRDA,
                file = files,
                MoreArgs = list(
                    "envir" = safe,
                    "overwrite" = FALSE
                )
            ))
            assert(areSetEqual(dots, ls(safe)))
            ## Now assign to the desired object names.
            invisible(Map(
                f = function(from, to, safe, envir) {
                    assign(
                        x = to,
                        value = get(from, envir = safe, inherits = FALSE),
                        envir = envir
                    )
                },
                from = dots,
                to = names(dots),
                MoreArgs = list("safe" = safe, "envir" = envir)
            ))
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
        invisible(files)
    }
