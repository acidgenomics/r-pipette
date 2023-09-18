#' Get remote URL directory listing
#'
#' @export
#' @note Updated 2023-09-18.
#'
#' @details
#' FTP and HTTP(S) servers are supported.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param type `character(1)`.
#' Type of files to return:
#' - `"all"`: Both directories and files.
#' - `"dirs"`: Directories only.
#' - `"files"`: Files only.
#'
#' @param pattern `character(1)`.
#' Regular expression pattern to use for matching.
#' Passes to `grepl` function internally.
#'
#' @param absolute `logical(1)`.
#' Return absolute path.
#'
#' @return `character`.
#' File basename, or absolute URL path when `absolute` is `TRUE`.
#'
#' @seealso
#' - `curlGetHeaders`.
#'
#' @examples
#' url <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
#' if (goalie::isAnExistingURL(url)) {
#'     x <- getURLDirList(url)
#'     print(x)
#' }
getURLDirList <- function(
        url,
        type = c("all", "dirs", "files"),
        pattern = NULL,
        absolute = FALSE
    ) {
    assert(
        requireNamespaces("RCurl"),
        isString(url),
        isMatchingRegex(x = url, pattern = "^(ftp|http|https)://"),
        isString(pattern, nullOK = TRUE),
        isFlag(absolute)
    )
    type <- match.arg(type)
    if (!isMatchingRegex(x = url, pattern = "/$")) {
        url <- paste0(url, "/") # nocov
    }
    assert(isAnExistingURL(url))
    destfile <- tempfile()
    status <- try(
        expr = {
            download.file(
                url = url,
                destfile = destfile,
                quiet = TRUE
            )
        },
        silent = TRUE
    )
    assert(
        !inherits(status, "try-error"),
        msg = sprintf("URL failure: {.url %s}.", url)
    )
    x <- import(con = destfile, format = "lines", quiet = TRUE)
    unlink(destfile)
    protocol <- strsplit(url, split = ":")[[1L]][[1L]]
    x <- switch(
        EXPR = protocol,
        "ftp" = {
            .ftpDirList(x = x, type = type)
        },
        "http" = {
            .httpDirList(x = x, type = type)
        },
        "https" = {
            .httpDirList(x = x, type = type)
        }
    )
    if (!hasLength(x)) {
        return(character())
    }
    if (isString(pattern)) {
        keep <- grepl(pattern = pattern, x = x)
        assert(
            hasLength(keep),
            msg = sprintf(
                "No files matched pattern {.var %s}.",
                pattern
            )
        )
        x <- x[keep]
    }
    if (isTRUE(absolute)) {
        x <- pasteURL(url, x)
    }
    x
}



#' Get list of files from an FTP server
#'
#' @note Updated 2023-09-18.
#' @noRd
#'
#' @param x `character`.
#' Source code lines from `download.file`.
#'
#' @return `character`.
#' File and directory basenames.
#'
#' @param type `character(1)`.
#' Type of files to return.
.ftpDirList <- function(x, type = c("all", "dirs", "files")) {
    assert(isCharacter(x))
    type <- match.arg(type)
    ## Reformat input as CSV.
    pattern <- paste0(
        "^",
        "([a-z-]{10})",
        "\\s+",
        "([0-9]+)",
        "\\s+",
        "([a-z]+)",
        "\\s+",
        "([^ ]+)",
        "\\s+",
        "([0-9]+)",
        "\\s+",
        "([^ ]+\\s+[^ ]+\\s+[^ ]+)",
        "\\s+",
        "(.+)",
        "$"
    )
    if (!all(grepl(pattern = pattern, x = x))) {
        return(character())
    }
    x <- sub(
        pattern = pattern,
        replacement = paste0(
            "\"\\1\"",
            ",",
            "\\2",
            ",",
            "\"\\3\"",
            ",",
            "\"\\4\"",
            ",",
            "\\5",
            ",",
            "\"\\6\"",
            ",",
            "\"\\7\""
        ),
        x = x
    )
    ## FIXME Need to add support for this in import.
    df <- read.csv(
        textConnection(x),
        header = FALSE,
        col.names = c(
            "perms",
            "n",
            "protocol",
            "user",
            "size",
            "date",
            "basename"
        )
    )
    ## Ensure we sanitize symlinks.
    df[["basename"]] <- sub(
        pattern = "\\s->\\s.+$",
        replacement = "",
        x = df[["basename"]]
    )
    ## Standardize modification date.
    df[["date"]] <- gsub(
        pattern = "\\s+",
        replacement = " ",
        x = df[["date"]]
    )
    df[["date"]] <- sub(
        pattern = "[0-9]{2}:[0-9]{2}",
        replacement = format(Sys.Date(), "%Y"),
        x = df[["date"]]
    )
    df[["date"]] <- as.Date(df[["date"]], format = "%b %d %Y")
    switch(
        EXPR = type,
        "dirs" = {
            keep <- grepl(pattern = "^d", x = df[["perms"]])
            if (!any(keep)) {
                return(character())
            }
            df <- df[keep, , drop = FALSE]
        },
        "files" = {
            keep <- !grepl(pattern = "^d", x = df[["perms"]])
            if (!any(keep)) {
                return(character())
            }
            df <- df[keep, , drop = FALSE]
        }
    )
    out <- sort(df[["basename"]])
    out
}



## FIXME Support files/directories filtering.

#' Get list of files from an HTTP(S) server
#'
#' @note Updated 2023-09-18.
#' @noRd
#'
#' @param x `character`.
#' Source code lines from `download.file`.
#'
#' @param type `character(1)`.
#' Type of files to return.
.httpDirList <- function(x, type = c("all", "dirs", "files")) {
    assert(isCharacter(x))
    type <- match.arg(type)
    if (!any(grepl(pattern = "^<h1>Index of", x = x))) {
        return(character())
    }
    ## Reformat input as CSV.
    pattern <- paste0(
        "^<a href=\".+\">(.+)</a>\\s+",
        "([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2})",
        "\\s+",
        "([^ ]+)",
        "\\s+",
        ".+$"
    )
    keep <- grepl(pattern = pattern, x = x)
    if (!any(keep)) {
        return(character())
    }
    x <- x[keep]
    x <- sub(
        pattern = pattern,
        replacement = "\"\\1\",\"\\2\",\"\\3\"",
        x = x
    )
    ## FIXME Need to add support for this in import.
    df <- read.csv(
        file = textConnection(x),
        header = FALSE,
        col.names = c("basename", "date", "size")
    )
    df[["date"]] <- as.POSIXlt(df[["date"]])
    switch(
        EXPR = type,
        "dirs" = {
            keep <- grepl(pattern = "/^", x = df[["basename"]])
            if (!any(keep)) {
                return(character())
            }
            df <- df[keep, , drop = FALSE]
        },
        "files" = {
            keep <- !grepl(pattern = "/^", x = df[["basename"]])
            if (!any(keep)) {
                return(character())
            }
            df <- df[keep, , drop = FALSE]
        }
    )
    df[["basename"]] <- sub(
        pattern = "/$",
        replacement = "",
        x = df[["basename"]]
    )
    out <- sort(df[["basename"]])
    out
}
