#' Transmit files from a remote FTP server
#'
#' Utility function that supports easy file matching and download from a remote
#' FTP server. Also enables on-the-fly file renaming and compression.
#'
#' @note Updated 2019-08-15.
#' @export
#'
#' @inheritParams acidroxygen::params
#' @inheritParams saveData
#' @param remoteDir `character(1)`.
#'   Remote FTP directory path.
#' @param localDir `character(1)`.
#'   Directory where to save files locally.
#' @param pattern `character(1)`.
#'   Pattern to match against remote file names.
#' @param rename `character(1)` or `NULL`.
#'   Rename the local files (including suffix), if desired.
#' @param compress `logical(1)`.
#'   gzip compress the files after download.
#'
#' @return Invisible `character`.
#' Local file paths.
#'
#' @examples
#' ## This doesn't work reliably on CI.
#' ## > remoteDir <- paste(
#' ## >     "ftp://ftp.pantherdb.org",
#' ## >     "sequence_classifications",
#' ## >     "current_release",
#' ## >     sep = "/"
#' ## > )
#' ## > readme <- transmit(
#' ## >     remoteDir = remoteDir,
#' ## >     pattern = "README",
#' ## >     rename = "panther_readme.txt",
#' ## >     compress = TRUE
#' ## > )
#' ## > basename(readme)
#' ## > file.exists(readme)
#' ## > unlink(readme)
transmit <- function(
    remoteDir,
    localDir = ".",
    pattern,
    rename = NULL,
    compress = FALSE
) {
    assert(
        hasInternet(),
        isString(remoteDir),
        ## Check for public FTP protocol. Note that we're wrapping with `all()`
        ## here because the check is parameterized, and includes name, which
        ## causes the check to fail on R 3.4.
        all(isMatchingRegex(remoteDir, "^ftp\\://"))
    )
    ## `RCurl::getURL()` requires a trailing slash.
    if (!grepl("/$", remoteDir)) {
        remoteDir <- paste0(remoteDir, "/")
    }
    localDir <- initDir(localDir)
    assert(
        isString(pattern),
        isAny(rename, classes = c("character", "NULL")),
        isFlag(compress)
    )

    ## Get the name of the server.
    server <- str_match(remoteDir, "^.*//([^/]+)/.*$")[1L, 2L]
    assert(isString(server))

    ## Error and inform the user if the FTP connection fails.
    if (!isTRUE(url.exists(remoteDir))) {
        stop(sprintf("Connection to '%s' failed.", server))  # nocov
    } else {
        message(sprintf("Transmitting files from '%s'.", server))
    }

    remoteTxt <- getURL(remoteDir)
    if (!all(
        is.character(remoteTxt),
        length(remoteTxt) > 0L
    )) {
        stop("Failed to list directory contents.")  # nocov
    }

    ## Match the `-` at begining for file.
    ## `-rwxrwxr-x`: File
    ## `drwxrwxr-x`: Directory
    remoteFiles <- unlist(
        x = strsplit(
            x = remoteTxt,
            split = "\n",
            fixed = TRUE
        ),
        recursive = FALSE,
        use.names = FALSE
    )
    remoteFiles <- remoteFiles[grepl("^-", remoteFiles)]
    ## File name is at the end, not including a space.
    remoteFiles <- str_extract(remoteFiles, "[^\\s]+$")
    assert(hasLength(remoteFiles))

    ## Apply pattern matching.
    match <- str_subset(remoteFiles, pattern)
    assert(hasLength(match))

    message(sprintf("Files matching pattern:\n%s", toString(match)))

    ## Concatenate using paste but strip the trailing slash (see above).
    remotePaths <- paste(gsub("/$", "", remoteDir), match, sep = "/")

    ## Rename files, if desired.
    if (is.character(rename)) {
        assert(areSameLength(x = match, y = rename))
        name <- rename
    } else {
        name <- match
    }

    localPaths <- file.path(localDir, name)

    if (isTRUE(compress)) {
        files <- paste(localPaths, "gz", sep = ".")
    } else {
        files <- localPaths
    }

    ## Check for existing files and skip, if necessary.
    if (any(file.exists(files))) {
        exists <- which(file.exists(files))
        skip <- files[exists]
        message(sprintf("Skipped: %s.", toString(basename(skip))))
        localPaths <- localPaths[!exists]
    }

    ## Early return if all files exist.
    if (length(localPaths) == 0L) {
        message("All files are already downloaded.")
        files <- realpath(files)
        names(files) <- match
        return(invisible(files))
    }

    message(sprintf("Downloading %s.", toString(basename(files))))
    files <- mapply(
        FUN = function(url, destfile, compress = FALSE) {
            download.file(url = url, destfile = destfile)
            if (isTRUE(compress)) {
                destfile <- gzip(destfile, overwrite = TRUE)
            }
            realpath(destfile)
        },
        url = remotePaths,
        destfile = localPaths,
        MoreArgs = list(compress = compress),
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
    names(files) <- match
    invisible(files)
}
