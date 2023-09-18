## FIXME Rework to use base R or our getURLDirList function instead.
## Remove the dependency on RCurl package.



#' Transmit files from a remote FTP server
#'
#' Utility function that supports easy file matching and download from a remote
#' FTP server. Also enables on-the-fly file renaming and compression.
#'
#' @export
#' @note Updated 2023-09-18.
#'
#' @details
#' Requires RCurl package to be installed.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams saveData
#'
#' @param remoteDir `character(1)`.
#' Remote FTP directory path.
#'
#' @param localDir `character(1)`.
#' Directory where to save files locally.
#'
#' @param rename `character(1)` or `NULL`.
#' Rename the local files (including suffix), if desired.
#'
#' @param compress `logical(1)`.
#' Compress the downloaded files.
#'
#' @param download `logical(1)`.
#' Download files (default) or only return the matching URL(s).
#'
#' @return Invisible `character`.
#' Local file paths.
#'
#' @examples
#' try({
#'     localDir <- AcidBase::tempdir2()
#'     readme <- transmit(
#'         remoteDir = "ftp://ftp.ncbi.nlm.nih.gov/genomes/",
#'         localDir = localDir,
#'         pattern = "^README\\.txt$",
#'         rename = "ncbi-readme.txt",
#'         compress = FALSE
#'     )
#'     basename(readme)
#'     file.exists(readme)
#'     AcidBase::unlink2(localDir)
#' })
transmit <-
    function(remoteDir,
             localDir = getwd(),
             pattern = NULL,
             rename = NULL,
             compress = FALSE,
             download = TRUE) {
        assert(
            requireNamespaces("RCurl"),
            ## We check that the URL exists after we add trailing slash below.
            isAURL(remoteDir),
            isMatchingRegex(x = remoteDir, pattern = "^ftp\\://"),
            isString(pattern, nullOK = TRUE),
            isAny(rename, classes = c("character", "NULL")),
            isFlag(compress),
            isFlag(download)
        )
        if (!isMatchingRegex(pattern = "/$", x = remoteDir)) {
            remoteDir <- paste0(remoteDir, "/")
        }
        ## Both `isAnExistingURL` and `getURL` require trailing slash.
        assert(isAnExistingURL(remoteDir))
        if (isTRUE(download)) {
            localDir <- initDir(localDir)
        }
        alert(sprintf("Transmitting files from {.url %s}.", remoteDir))
        ## Get a list of the files in the remote directory.
        remoteTxt <- RCurl::getURL(url = remoteDir)
        assert(
            isString(remoteTxt),
            msg = "Failed to list directory contents."
        )
        ## Match the `-` at begining for file.
        ## `-rwxrwxr-x`: File
        ## `drwxrwxr-x`: Directory
        remoteFiles <- strsplit(
            x = remoteTxt,
            split = "\n",
            fixed = TRUE
        )[[1L]]
        ## Match files but not dirs (`"^d"`) or symlinks (`"^l"`).
        remoteFiles <-
            remoteFiles[grepl(pattern = "^-", x = remoteFiles)]
        remoteFiles <- strsplit(
            x = remoteFiles,
            split = "[[:space:]]+",
            fixed = FALSE
        )
        remoteFiles <- do.call(what = rbind, args = remoteFiles)
        remoteFiles <- remoteFiles[, 9L]
        assert(hasLength(remoteFiles))
        ## Apply pattern matching.
        if (!is.null(pattern)) {
            remoteFiles <- grep(
                pattern = pattern,
                x = remoteFiles,
                ignore.case = FALSE,
                value = TRUE,
                fixed = FALSE
            )
            assert(
                hasLength(remoteFiles),
                msg = sprintf(
                    "No files matching pattern: {.var %s}.",
                    pattern
                )
            )
            alertInfo(sprintf(
                "%d %s matching pattern: %s.",
                length(remoteFiles),
                ngettext(
                    n = length(remoteFiles),
                    msg1 = "file",
                    msg2 = "files"
                ),
                toInlineString(remoteFiles, n = 10L, class = "file")
            ))
        }
        remotePaths <- pasteURL(remoteDir, remoteFiles, protocol = "none")
        if (isFALSE(download)) {
            return(remotePaths)
        }
        ## Rename files, if desired.
        if (!is.null(rename)) {
            assert(areSameLength(x = remoteFiles, y = rename))
            name <- rename
        } else {
            name <- remoteFiles
        }
        localPaths <- file.path(localDir, name)
        if (isTRUE(compress)) {
            files <- paste(localPaths, "gz", sep = ".")
        } else {
            files <- localPaths
        }
        ## Check for existing files and skip, if necessary.
        if (any(file.exists(files))) {
            exists <- file.exists(files)
            skip <- files[exists]
            alertWarning(sprintf(
                "Skipped: %s.",
                toInlineString(basename(skip), n = 10L, class = "file")
            ))
            localPaths <- localPaths[!exists]
        }
        ## Early return if all files exist.
        if (!hasLength(localPaths)) {
            alertSuccess("All files are already downloaded.")
            files <- realpath(files)
            names(files) <- remoteFiles
            return(invisible(files))
        }
        ## Download and return file paths.
        alert(sprintf(
            "Downloading %s.",
            toInlineString(basename(files), n = 10L, class = "file")
        ))
        files <- Map(
            f = function(url, destfile, compress = FALSE) {
                download(url = url, destfile = destfile)
                if (isTRUE(compress)) {
                    destfile <- compress(
                        file = destfile,
                        remove = TRUE,
                        overwrite = TRUE
                    )
                }
                realpath(destfile)
            },
            url = remotePaths,
            destfile = localPaths,
            MoreArgs = list(compress = compress),
            USE.NAMES = FALSE
        )
        files <- unlist(files, recursive = FALSE, use.names = FALSE)
        names(files) <- remoteFiles
        invisible(files)
    }
