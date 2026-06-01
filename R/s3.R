#' Check if a string is an S3 URI
#'
#' @note Updated 2026-05-31.
#' @noRd
.isS3Uri <- function(x) {
    assert(isString(x))
    grepl(pattern = "^s3://", x = x, fixed = FALSE)
}



#' Parse an S3 URI into bucket and key components
#'
#' @note Updated 2026-05-31.
#' @noRd
.s3ParseUri <- function(uri) {
    assert(isString(uri), .isS3Uri(uri))
    ## Strip the "s3://" prefix.
    path <- sub(
        pattern = "^s3://",
        replacement = "",
        x = uri,
        fixed = FALSE
    )
    ## Split on the first "/" to separate bucket from key.
    idx <- regexpr(pattern = "/", text = path, fixed = TRUE)
    if (identical(idx[[1L]], -1L)) {
        ## No key — bucket only.
        return(list("bucket" = path, "key" = ""))
    }
    bucket <- substr(path, start = 1L, stop = idx[[1L]] - 1L)
    key <- substr(path, start = idx[[1L]] + 1L, stop = nchar(path))
    list("bucket" = bucket, "key" = key)
}



#' Download a file from S3 to a local temp file
#'
#' @note Updated 2026-05-31.
#' @noRd
.s3Download <- function(uri, quiet = FALSE) {
    assert(
        isString(uri),
        .isS3Uri(uri),
        isFlag(quiet),
        requireNamespaces("paws.storage")
    )
    if (isFALSE(quiet)) {
        alert(sprintf("Downloading from S3: {.url %s}.", uri))
    }
    parsed <- .s3ParseUri(uri)
    bucket <- parsed[["bucket"]]
    key <- parsed[["key"]]
    assert(
        isString(bucket),
        isString(key)
    )
    fileExt <- fileExt(key)
    tmpFileExt <- ifelse(
        test = is.na(fileExt),
        yes = "",
        no = paste0(".", fileExt)
    )
    tmpFile <- tempfile(
        pattern = paste0(.pkgName, "-"),
        tmpdir = tempdir2(),
        fileext = tmpFileExt
    )
    svc <- paws.storage::s3()
    resp <- svc$get_object(Bucket = bucket, Key = key)
    writeBin(resp[["Body"]], con = tmpFile)
    assert(isAFile(tmpFile))
    tmpFile
}



#' Create a local temp path mirroring the extension of an S3 URI
#'
#' @note Updated 2026-05-31.
#' @noRd
.s3TempPath <- function(uri) {
    assert(isString(uri), .isS3Uri(uri))
    ext <- fileExt(uri)
    tmpFileExt <- ifelse(
        test = is.na(ext),
        yes = "",
        no = paste0(".", ext)
    )
    tempfile(
        pattern = paste0(.pkgName, "-export-"),
        tmpdir = tempdir2(),
        fileext = tmpFileExt
    )
}



#' Upload a local file to S3
#'
#' @note Updated 2026-05-31.
#' @noRd
.s3Upload <- function(file, uri, quiet = FALSE) {
    assert(
        isAFile(file),
        isString(uri),
        .isS3Uri(uri),
        isFlag(quiet),
        requireNamespaces("paws.storage")
    )
    if (isFALSE(quiet)) {
        alert(sprintf("Uploading to S3: {.url %s}.", uri))
    }
    parsed <- .s3ParseUri(uri)
    bucket <- parsed[["bucket"]]
    key <- parsed[["key"]]
    assert(
        isString(bucket),
        isString(key)
    )
    body <- readBin(
        con = file,
        what = "raw",
        n = file.info(file)[["size"]]
    )
    svc <- paws.storage::s3()
    svc$put_object(Body = body, Bucket = bucket, Key = key)
    invisible(uri)
}
