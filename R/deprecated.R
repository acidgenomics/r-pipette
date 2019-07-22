## nocov start
## nolint start



#' Deprecated functions
#'
#' @name deprecated
#' @keywords internal
#'
#' @inheritParams params
#'
#' @return `.Deprecated`.
NULL



#' Defunct functions
#'
#' @name defunct
#' @keywords internal
#'
#' @inheritParams params
#'
#' @return `.Defunct`.
NULL



## v0.1.4 =======================================================================
## Deprecate these sanitization functions.
## Still in use by bcbio R packages.
## Consider switching to soft deprecation if too noisy.

#' @rdname deprecated
#' @export
sanitizeColData <- function(...) {
    .Deprecated("atomize")
    atomize(...)
}

#' @rdname deprecated
#' @export
sanitizeRowData <- function(...) {
    .Deprecated("atomize")
    atomize(...)
}

#' @rdname deprecated
#' @export
sanitizeRowRanges <- function(...) {
    .Deprecated("atomize")
    atomize(...)
}



## nolint end
## nocov end
