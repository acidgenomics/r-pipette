#' Load remote data
#'
#' Load a remote R binary file. This function is vectorized and supports
#' multiple URLs in a single call.
#'
#' @export
#' @note Updated 2022-05-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param url `character`.
#' Remote URL file path(s) to R data.
#'
#' @return Invisible named `character`.
#' Local object name as the name, and the remote URL as the value.
#'
#' @examples
#' url <- AcidBase::pasteURL(
#'     pipetteTestsURL,
#'     "rnaseq_counts.rds",
#'     protocol = "none"
#' )
#' print(url)
#' x <- loadRemoteData(url)
#' print(x)
loadRemoteData <-
    function(url,
             envir = globalenv(),
             overwrite = getOption(
                 x = "acid.overwrite",
                 default = TRUE
             )) {
        assert(
            allAreExistingURLs(url),
            is.environment(envir),
            isFlag(overwrite)
        )
        assert(
            allAreMatchingRegex(
                x = tolower(basename(url)),
                pattern = .rdataExtPattern
            ),
            msg = .rdataLoadError
        )
        names <- gsub(
            pattern = .rdataExtPattern,
            replacement = "",
            x = basename(url),
            ignore.case = TRUE
        )
        names(url) <- names
        ## Check to make sure the objects don't already exist.
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
        ## Download the files to tempdir and return a mapping character matrix.
        invisible(Map(
            name = names,
            url = url,
            MoreArgs = list("envir" = envir),
            f = function(name, url, envir) {
                data <- import(url)
                assign(x = name, value = data, envir = envir)
            }
        ))
        assert(allAreExisting(names, envir = envir, inherits = FALSE))
        invisible(url)
    }
