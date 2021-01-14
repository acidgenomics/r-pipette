#' Download and cache a file using BiocFileCache
#'
#' @export
#' @note Updated 2021-01-14.
#'
#' @inheritParams AcidRoxygen::params
#' @param fileName `character(1)`.
#'   File name to store internally in `BiocFileCache`.
#'   Defaults to basename of URL.
#' @param pkg `character(1)`.
#'   Package name.
#' @param update `logical(1)`.
#'   Call `bfcneedsupdate` internally to see if URL needs an update.
#'   Doesn't work reliably for all servers, so disabled by default.
#' @param ask `logical(1)`.
#'   Ask if package cache needs to be created or if file needs to be updated.
#'
#' @return `character(1)`.
#'   Cached file path on disk.
#'
#' @examples
#' url <- pasteURL(
#'     pipetteTestsURL,
#'     "biocfilecache-test.txt",
#'     protocol = "none"
#' )
#' file <- cacheURL(url)
#' print(file)
cacheURL <- function(
    url,
    fileName = basename(url),
    pkg = packageName(),
    update = FALSE,
    ask = FALSE,
    verbose = TRUE
) {
    assert(
        hasInternet(),
        isAURL(url),
        isString(fileName),
        isString(pkg),
        isFlag(update),
        isFlag(ask),
        isFlag(verbose)
    )
    bfc <- .biocPackageCache(pkg = pkg, ask = ask)
    rid <- bfcquery(
        x = bfc,
        query = fileName,
        field = "rname",
        exact = TRUE
    )[["rid"]]
    if (!hasLength(rid)) {
        ## nocov start
        if (isTRUE(verbose)) {
            alert(sprintf(
                "Caching URL at {.url %s} into {.path %s}.",
                url, bfccache(bfc)
            ))
        }
        rid <- names(bfcadd(
            x = bfc,
            rname = fileName,
            fpath = url,
            download = TRUE
        ))
        ## nocov end
    }
    if (isTRUE(update)) {
        ## nocov start
        if (!isFALSE(bfcneedsupdate(x = bfc, rids = rid))) {
            bfcdownload(x = bfc, rid = rid, ask = ask)
        }
        ## nocov end
    }
    out <- unname(bfcrpath(x = bfc, rids = rid))
    assert(isAFile(out))
    out
}



#' Prepare BiocFileCache for package
#'
#' @note Updated 2020-10-06.
#' @noRd
.biocPackageCache <- function(pkg = packageName(), ask = TRUE) {
    assert(isString(pkg), isFlag(ask))
    BiocFileCache(cache = user_cache_dir(appname = pkg), ask = ask)
}
