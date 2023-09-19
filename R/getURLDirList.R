#' Get remote URL directory listing
#'
#' @export
#' @note Updated 2023-09-19.
#'
#' @details
#' FTP and HTTP(S) servers are supported.
#' Designed to be simple, and does not support recursive directory listing.
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
#' Only matches against the basename of the URL.
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
getURLDirList <-
    function(url,
             type = c("all", "dirs", "files"),
             pattern = NULL,
             absolute = FALSE) {
        assert(
            isString(url),
            isMatchingRegex(x = url, pattern = "^(ftp|http|https)://"),
            isString(pattern, nullOK = TRUE),
            isFlag(absolute)
        )
        type <- match.arg(type)
        if (!isMatchingRegex(x = url, pattern = "/$")) {
            url <- paste0(url, "/")
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
        assert(isCharacter(x))
        if (!hasLength(x)) {
            return(character())
        }
        x <- sort(x)
        if (isString(pattern)) {
            keep <- grepl(pattern = pattern, x = x)
            assert(
                any(keep),
                msg = sprintf(
                    "No files matched pattern {.var %s}.",
                    pattern
                )
            )
            x <- x[keep]
        }
        if (isTRUE(absolute)) {
            x <- paste0(url, x)
        }
        x
    }



#' Get list of files from an FTP server
#'
#' @note Updated 2023-09-19.
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
.ftpDirList <-
    function(x, type = c("all", "dirs", "files")) {
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
        con <- textConnection(x)
        df <- import(
            con = con,
            format = "csv",
            colnames = c(
                "perms",
                "n",
                "protocol",
                "user",
                "size",
                "date",
                "basename"
            ),
            quiet = TRUE
        )
        close(con)
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
        out <- df[["basename"]]
        out
    }



#' Get list of files from an HTTP(S) server
#'
#' @note Updated 2023-09-19.
#' @noRd
#'
#' @param x `character`.
#' Source code lines from `download.file`.
#'
#' @param type `character(1)`.
#' Type of files to return.
.httpDirList <-
    function(x, type = c("all", "dirs", "files")) {
        assert(isCharacter(x))
        type <- match.arg(type)
        if (!any(grepl(pattern = "^<h1>Index of", x = x))) {
            return(character())
        }
        ## Reformat input as CSV.
        ## Loop across different HTTP server listings until we hit a match.
        patterns <- c(
            "ncbi" = paste0(
                "^<a href=\".+\">(.+)</a>\\s+",
                "([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2})",
                "\\s+",
                "([^ ]+)",
                "\\s+",
                ".+$"
            ),
            "ensembl" = paste0(
                "^<tr>",
                ".+",
                "<td><a href=\".+\">(.+)</a></td>",
                "<td.+>(.+)</td>",
                "<td.+>(.+)</td>",
                ".+",
                "</tr>$"
            )
        )
        keep <- logical()
        pattern <- character()
        i <- 0L
        while (i < length(patterns)) {
            i <- i + 1L
            keep <- grepl(pattern = patterns[[i]], x = x)
            if (any(keep)) {
                pattern <- patterns[[i]]
                break
            }
        }
        if (!any(keep)) {
            return(character())
        }
        x <- x[keep]
        x <- sub(
            pattern = pattern,
            replacement = "\"\\1\",\"\\2\",\"\\3\"",
            x = x
        )
        con <- textConnection(x)
        df <- import(
            con = con,
            format = "csv",
            colnames = c("basename", "date", "size"),
            quiet = TRUE
        )
        close(con)
        df[["date"]] <- as.POSIXlt(df[["date"]])
        switch(
            EXPR = type,
            "dirs" = {
                keep <- grepl(pattern = "/$", x = df[["basename"]])
                if (!any(keep)) {
                    return(character())
                }
                df <- df[keep, , drop = FALSE]
            },
            "files" = {
                keep <- !grepl(pattern = "/$", x = df[["basename"]])
                if (!any(keep)) {
                    return(character())
                }
                df <- df[keep, , drop = FALSE]
            }
        )
        ## Ensure directories don't return with trailing slash, similar to FTP.
        df[["basename"]] <- sub(
            pattern = "/$",
            replacement = "",
            x = df[["basename"]]
        )
        out <- df[["basename"]]
        out
    }
