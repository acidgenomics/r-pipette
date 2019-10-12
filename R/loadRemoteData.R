#' Load remote data
#'
#' Load a remote R binary file. This function is vectorized and supports
#' multiple URLs in a single call.
#'
#' @export
#' @note Updated 2019-10-12.
#'
#' @inheritParams acidroxygen::params
#' @param url `character`.
#'   Remote URL file path(s) to R data.
#'
#' @return Invisible named `character`.
#' Local object name as the name, and the remote URL as the value.
#'
#' @examples
#' url <- pasteURL(brioTestsURL, "rnaseq_counts.rds", protocol = "none")
#' print(url)
#' x <- loadRemoteData(url)
#' print(x)
loadRemoteData <- function(
    url,
    envir = globalenv(),
    overwrite
) {
    assert(
        hasInternet(),
        allAreURLs(url),
        is.environment(envir),
        isFlag(overwrite)
    )
    if (!all(bapply(
        X = url,
        FUN = function(x) {
            grepl(pattern = .rdataExtPattern, x = x, ignore.case = TRUE)
        }
    ))) {
        stop(.rdataLoadError)
    }
    names <- gsub(
        pattern = .rdataExtPattern,
        replacement = "",
        x = basename(url),
        ignore.case = TRUE
    )
    names(url) <- names

    ## Check to make sure the objects don't already exist.
    if (
        !isTRUE(overwrite) &&
        !isTRUE(allAreNonExisting(names, envir = envir, inherits = FALSE))
    ) {
        .loadExistsError(names)
    }

    ## Download the files to tempdir and return a character matrix of mappings.
    invisible(mapply(
        name = names,
        url = url,
        MoreArgs = list(envir = envir),
        FUN = function(name, url, envir) {
            data <- import(url)
            assign(x = name, value = data, envir = envir)
        }
    ))

    assert(allAreExisting(names, envir = envir, inherits = FALSE))
    invisible(url)
}

formals(loadRemoteData)[["overwrite"]] <- .formalsList[["overwrite"]]
