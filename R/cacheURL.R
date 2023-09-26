#' Download and cache a file
#'
#' @export
#' @note Updated 2023-09-26.
#'
#' @details
#' Caching requires BiocFileCache and tools packages to be installed. If
#' BiocFileCache is not installed, URL will be downloaded as a temporary file
#' instead.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param pkg `character(1)`.
#' Package name.
#'
#' @param update `logical(1)`.
#' Call `bfcneedsupdate` internally to see if URL needs an update.
#' Doesn't work reliably for all servers, so disabled by default.
#'
#' @param ask `logical(1)`.
#' Ask if package cache needs to be created or if file needs to be updated.
#'
#' @return `character(1)`.
#' Cached file path on disk.
#'
#' @seealso
#' - `BiocFileCache::bfcinfo()`.
#'
#' @examples
#' url <- AcidBase::pasteURL(pipetteTestsURL, "biocfilecache-test.txt")
#' file <- cacheURL(url)
#' print(file)
cacheURL <-
    function(url,
             pkg = "pipette",
             update = FALSE,
             ask = FALSE,
             verbose = TRUE) {
        assert(
            isAnExistingURL(url),
            isString(pkg),
            isFlag(update),
            isFlag(ask),
            isFlag(verbose)
        )
        ## Download as a temporary file if BiocFacheCache is not installed.
        if (!isInstalled("BiocFileCache")) {
            destfile <- file.path(tempdir2(), basename(url))
            download.file(url = url, destfile = destfile, quiet = !verbose)
            return(destfile)
        }
        bfc <- .biocPackageCache(pkg = pkg, ask = ask)
        query <- BiocFileCache::bfcquery(
            x = bfc,
            query = url,
            field = "fpath",
            exact = TRUE
        )
        rid <- query[["rid"]]
        if (!hasLength(rid)) {
            if (isTRUE(verbose)) {
                alert(sprintf(
                    "Caching URL at {.url %s} into {.path %s}.",
                    url,
                    BiocFileCache::bfccache(bfc)
                ))
            }
            add <- BiocFileCache::bfcadd(
                x = bfc,
                rname = basename(url),
                fpath = url,
                download = TRUE
            )
            rid <- names(add)[[1L]]
            assert(isString(rid))
        }
        if (isTRUE(update)) {
            ## Note that some servers will return NA here, which isn't helpful.
            up <- BiocFileCache::bfcneedsupdate(x = bfc, rids = rid)
            if (isTRUE(up)) {
                BiocFileCache::bfcdownload(x = bfc, rid = rid, ask = ask)
            }
        }
        rpath <- BiocFileCache::bfcrpath(x = bfc, rids = rid)
        assert(isAFile(rpath))
        out <- unname(rpath)
        out
    }



#' Prepare BiocFileCache for package
#'
#' @note Updated 2023-09-14.
#' @noRd
#'
#' @seealso
#' - `Sys.getenv("R_USER_CACHE_DIR")`.
#' - `Sys.getenv("XDG_CACHE_HOME")`.
#' - `rappdirs::user_cache_dir()`.
.biocPackageCache <- function(pkg, ask) {
    assert(
        requireNamespaces(c("BiocFileCache", "tools")),
        isString(pkg),
        isFlag(ask)
    )
    cache <- tools::R_user_dir(package = pkg, which = "cache")
    bfc <- BiocFileCache::BiocFileCache(cache = cache, ask = ask)
    bfc
}
