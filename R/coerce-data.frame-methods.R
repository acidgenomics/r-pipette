#' Coerce to data frame
#'
#' Coerce to `data.frame`.
#'
#' @name coerce-data.frame
#' @note Updated 2020-01-19.
#'
#' @inheritParams base::as.data.frame
#' @inheritParams AcidRoxygen::params
#'
#' @return `data.frame`.
#'
#' @examples
#' data(sparseMatrix, package = "AcidTest")
#'
#' ## Matrix to data.frame ====
#' x <- as(sparseMatrix, "data.frame")
#' head(x)
NULL



## Default coercion of IPosRanges (i.e. IRanges) to data.frame currently
## strips metadata in `mcols()`. However, GenomicRanges preserves this
## information, so we're adding a tweaked coercion method here to improve
## consistency.
##
## Relevant methods:
## > getMethod(
## >     f = "as.data.frame",
## >     signature = "GenomicRanges",
## >     where = asNamespace("GenomicRanges")
## > )
## IRanges inherits from `IPosRanges`.
## > getMethod(
## >     f = "as.data.frame",
## >     signature = "IPosRanges",
## >     where = asNamespace("IRanges")
## > )
##
## See also:
## - https://github.com/Bioconductor/IRanges/issues/8
##
## Updated 2020-01-19.
`as.data.frame,IRanges` <-  # nolint
    function(
        x,
        row.names = NULL,
        optional = FALSE,
        ...
    ) {
        if (missing(row.names)) {
            row.names <- names(x)
        }
        if (!is.null(names(x))) {
            names(x) <- NULL
        }
        args <- list(
            start = start(x),
            end = end(x),
            width = width(x),
            row.names = row.names,
            check.rows = TRUE,
            check.names = FALSE,
            stringsAsFactors = FALSE
        )
        mcols <- mcols(x, use.names = FALSE)
        if (!is.null(mcols)) {
            args[["mcols"]] <- as.data.frame(mcols)
        }
        do.call(what = data.frame, args = args)
    }



#' @rdname coerce-data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("IRanges"),
    definition = `as.data.frame,IRanges`
)



## Updated 2019-07-20.
`as.data.frame,Matrix` <-  # nolint
    function(x, ...) {
        as.data.frame(as.matrix(x), ...)
    }



#' @rdname coerce-data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("Matrix"),
    definition = `as.data.frame,Matrix`
)



`coerce,ANY,data.frame` <-  # nolint
    function(from) {
        as.data.frame(from)
    }



## Updated 2019-07-19.
`coerce,IRanges,data.frame` <-  # nolint
    `coerce,ANY,data.frame`



#' @rdname coerce-data.frame
#' @name coerce,IRanges,data.frame-method
setAs(
    from = "IRanges",
    to = "data.frame",
    def = `coerce,IRanges,data.frame`
)



## Updated 2019-07-20.
`coerce,Matrix,data.frame` <-  # nolint
    `coerce,ANY,data.frame`



#' @rdname coerce-data.frame
#' @name coerce,Matrix,data.frame-method
setAs(
    from = "Matrix",
    to = "data.frame",
    def = `coerce,Matrix,data.frame`
)
