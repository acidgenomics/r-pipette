## nocov start
## nolint start



#' @name defunct
#' @inherit acidroxygen::defunct description examples return seealso title
#' @inheritParams acidroxygen::params
#' @keywords internal
NULL



#' @name deprecated
#' @inherit acidroxygen::deprecated description examples return seealso title
#' @inheritParams acidroxygen::params
#' @keywords internal
NULL



## v0.1.4 =======================================================================
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
