#' Download and cache a file using BiocFileCache
#'
#' @export
#' @note Updated 2023-08-24.
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
#' url <- AcidBase::pasteURL(
#'     pipetteTestsURL,
#'     "biocfilecache-test.txt",
#'     protocol = "none"
#' )
#' file <- cacheURL(url)
#' print(file)
cacheURL <-
    function(url,
             pkg = "BiocFileCache",
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
        bfc <- .biocPackageCache(pkg = pkg, ask = ask)
        query <- bfcquery(
            x = bfc,
            query = url,
            field = "fpath",
            exact = TRUE
        )
        rid <- query[["rid"]]
        ## nocov start
        if (!hasLength(rid)) {
            if (isTRUE(verbose)) {
                alert(sprintf(
                    "Caching URL at {.url %s} into {.path %s}.",
                    url, bfccache(bfc)
                ))
            }
            add <- bfcadd(
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
            up <- bfcneedsupdate(x = bfc, rids = rid)
            if (isTRUE(up)) {
                bfcdownload(x = bfc, rid = rid, ask = ask)
            }
        }
        ## nocov end
        rpath <- bfcrpath(x = bfc, rids = rid)
        assert(isAFile(rpath))
        out <- unname(rpath)
        out
    }



#' Prepare BiocFileCache for package
#'
#' @note Updated 2023-08-24.
#' @noRd
#'
#' @seealso
#' - `Sys.getenv("R_USER_CACHE_DIR")`.
#' - `Sys.getenv("XDG_CACHE_HOME")`.
#' - `rappdirs::user_cache_dir()`.
.biocPackageCache <- function(pkg, ask) {
    assert(
        isString(pkg),
        isFlag(ask)
    )
    cache <- R_user_dir(package = pkg, which = "cache")
    bfc <- BiocFileCache(cache = cache, ask = ask)
    bfc
}
