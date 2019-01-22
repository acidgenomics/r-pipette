# nocov start
# nolint start



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



# v0.1.4 =======================================================================
#' @rdname defunct
#' @export
sanitizeColData <- function(...) {
    .Defunct("atomize")
}

#' @rdname defunct
#' @export
sanitizeRowData <- function(...) {
    .Defunct("atomize")
}

#' @rdname defunct
#' @export
sanitizeRowRanges <- function(...) {
    .Defunct("atomize")
}



# nolint end
# nocov end
