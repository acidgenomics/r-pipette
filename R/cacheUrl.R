#' Download and cache a file
#'
#' @export
#' @note Updated 2023-09-28.
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
#' url <- AcidBase::pasteUrl(pipetteTestsUrl, "biocfilecache-test.txt")
#' file <- cacheUrl(url)
#' print(file)
cacheUrl <-
    function(url,
             pkg = "pipette",
             update = FALSE,
             ask = FALSE,
             verbose = TRUE) {
        assert(
            isAnExistingUrl(url),
            isString(pkg),
            isFlag(update),
            isFlag(ask),
            isFlag(verbose)
        )
        ## Download as a temporary file if BiocFacheCache is not installed.
        if (!isInstalled("BiocFileCache")) {
            cacheDir <- file.path(pkgCacheDir(pkg), randomString())
            cacheDir <- initDir(cacheDir)
            destfile <- file.path(cacheDir, basename(url))
            download(url = url, destfile = destfile, quiet = !verbose)
            assert(isAFile(destfile))
            return(destfile)
        }
        bfc <- .biocPkgCache(pkg = pkg, ask = ask)
        query <- BiocFileCache::bfcquery(
            x = bfc, query = url, field = "fpath", exact = TRUE
        )
        rid <- query[["rid"]]
        if (!hasLength(rid)) {
            if (isTRUE(verbose)) {
                alert(sprintf(
                    "Caching URL at {.url %s} into {.path %s}.",
                    url, BiocFileCache::bfccache(bfc)
                ))
            }
            add <- BiocFileCache::bfcadd(
                x = bfc, rname = basename(url), fpath = url, download = TRUE
            )
            rid <- names(add)[[1L]]
            assert(isString(rid))
        }
        if (isTRUE(update)) {
            ## Some servers will return NA here, which isn't helpful.
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
#' @note Updated 2023-09-28.
#' @noRd
#'
#' @seealso
#' - `AcidBase::cacheDir()`.
.biocPkgCache <- function(pkg, ask) {
    assert(
        requireNamespaces(c("BiocFileCache", "tools")),
        isString(pkg),
        isFlag(ask)
    )
    cache <- initDir(file.path(pkgCacheDir(pkg), "BiocFileCache"))
    bfc <- BiocFileCache::BiocFileCache(cache = cache, ask = ask)
    bfc
}
