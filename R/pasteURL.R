#' Concatenate strings to form a URL
#'
#' @export
#' @note Updated 2019-10-18.
#'
#' @inheritParams base::paste
#' @param protocol `character(1)`.
#'   Desired protocol to use. Defaults to HTTPS but HTTP and FTP are also
#'   supported. Use `"none"` if you want to prepare a URL that already contains
#'   a protocol in the first element of the dots.
#'
#' @return `character`.
#' URL path, containing forward slashes (`/`).
#'
#' @examples
#' ## HTTPS
#' x <- pasteURL(
#'     "steinbaugh.com",
#'     "basejump",
#'     "reference",
#'     protocol = "https"
#' )
#' print(x)
#'
#' ## FTP
#' x <- pasteURL(
#'     "ftp.ensembl.org",
#'     "pub",
#'     "release-94",
#'     "gtf",
#'     "homo_sapiens",
#'     "Homo_sapiens.GRCh38.94.gtf.gz",
#'     protocol = "ftp"
#' )
#' print(x)
pasteURL <- function(..., protocol = c("https", "http", "ftp", "none")) {
    dots <- unlist(list(...))
    assert(isCharacter(dots))
    protocol <- match.arg(protocol)
    dots <- gsub(pattern = "/$", replacement = "", x = dots)
    url <- paste(dots, collapse = "/")
    assert(isString(url))
    if (protocol != "none") {
        url <- paste0(protocol, "://", url)
    }
    assert(isAURL(url))
    url
}
