## This is needed to properly declare S4 `as()` coercion methods.
#' @name coerce
#' @export
#' @importFrom methods coerce
#' @exportMethod coerce
NULL



#' Coercion methods
#'
#' @name coerce
#' @note Updated 2020-05-18.
#'
#' @details
#' These conversion methods are primarily intended to interconvert between
#' popular tabular formats in R, including `data.frame`, `data.table`, `tbl_df`,
#' and the Bioconductor `DFrame` classes.
#'
#' @section `DataFrame` / `DFrame` (Bioconductor) coercion:
#'
#' Don't define `as()` coercion method for `list` here. It will create issues
#' with `data.frame` coercion. Use `as.DataFrame()` instead to coerce a `list`
#' to `DFrame`.
#'
#' Wrapping the columns in an `I()` works when passing to `DataFrame()`.
#' See also `as_tibble()` for easy list to data frame coercion.
#'
#' `as()` method definition causes issues with `data.frame` to `DFrame`
#' coercion when defined, because `data.frame` inherits from list.
#'
#' @section `data.frame` coercion:
#'
#' ## To `IRanges`
#'
#' Default coercion of `IPosRanges` (i.e. `IRanges`) to `data.frame` currently
#' strips metadata in `mcols()`. However, GenomicRanges preserves this
#' information, so we're adding a tweaked coercion method here to improve
#' consistency.
#'
#' Relevant methods:
#'
#' ```r
#' getMethod(
#'     f = "as.data.frame",
#'     signature = "GenomicRanges",
#'     where = asNamespace("GenomicRanges")
#' )
#' ## IRanges inherits from `IPosRanges`.
#' getMethod(
#'     f = "as.data.frame",
#'     signature = "IPosRanges",
#'     where = asNamespace("IRanges")
#' )
#' ```
#'
#' See also:
#' - https://github.com/Bioconductor/IRanges/issues/8
#'
#' @section `data.table` coercion:
#'
#' Our defined methods attempt to improve on the defaults in the data.table
#' package to ensure that row names are not dropped by default, which is a poor
#' default for bioinformatics. This is accomplished by setting
#' `keep.rownames = "rowname"` by default instead of `keep.rownames = NULL`.
#' Note that we're manually defining the `"rowname"` column instead of using
#' `TRUE`, to match the conventions used in our `as_tibble()` methods.
#'
#' ## S3 methods: `as.data.table()`
#'
#' The package extends `as.data.table()` method support for these S4 classes:
#'
#' - `DFrame` (from S4Vectors package).
#' - `GRanges` (from GenomicRanges package).
#'
#' ## S4 methods: `as()`
#'
#' Since `data.table` is a class that extends `data.frame`, we need to define an
#' S4 coercion method that allows us to use `as()` to coerce an object to a
#' `data.table`.
#'
#' @section tibble (`tbl_df`) coercion:
#'
#' Our defined methods attempt to improve on the defaults in the tibble package
#' to ensure that row names are not dropped by default, which is a poor default
#' for bioinformatics. This is accomplished by setting `rownames = "rowname"` by
#' default instead of `rownames = NULL`.
#'
#' Note that we're matching `as_tibble()` convention here, using `rowname` as
#' column for row names assignment. We also using similar internal assert checks
#' here, allowing atomic and/or list columns only.
#'
#' ## S3 methods: `as_tibble()`
#'
#' The package extends `as_tibble()` method support for these S4 classes:
#'
#' - `DFrame` (from S4Vectors package).
#' - `GRanges` (from GenomicRanges package).
#'
#' ## S4 methods: `as()`
#'
#' Since `tbl_df` is a virtual class that extends `tbl` and `data.frame`, we
#' need to define an S4 coercion method that allows us to use `as()` to coerce
#' an object to a tibble.
#'
#' @inheritParams base::as.data.frame
#' @inheritParams data.table::as.data.table
#' @inheritParams AcidRoxygen::params
#' @param row.names `NULL` or `character`.
#' @param ... Additional arguments.
#'
#' @return Modified object, of desired conversion class.
#'
#' @seealso
#' - `as.data.frame()`.
#' - `as.data.table()`.
#' - `as_tibble()`.
#' - `getClass("DFrame")`.
#' - `getClass("data.table")`.
#' - `getClass("tbl_df")`.
#'
#' @examples
#' data(
#'     DFrame,
#'     GRanges,
#'     IRanges,
#'     data.table,
#'     sparseMatrix,
#'     tbl_df,
#'     package = "AcidTest"
#' )
#'
#' ## `DFrame` to `data.table` ====
#' x <- as(DFrame, "data.table")
#' x <- as.data.table(DFrame)
#' print(x)
#'
#' ## `DFrame` to `tbl_df` ====
#' x <- as(DFrame, "tbl_df")
#' x <- as_tibble(DFrame)
#' print(x)
#'
#' ## `GRanges` to `data.table` ====
#' x <- as(GRanges, "data.table")
#' x <- as.data.table(GRanges)
#' print(x)
#'
#' ## `GRanges` to `tbl_df` ====
#' x <- as(GRanges, "tbl_df")
#' x <- as_tibble(GRanges)
#' print(x)
#'
#' ## `IRanges` to `data.table` ====
#' x <- as(IRanges, "data.table")
#' x <- as.data.table(IRanges)
#' print(x)
#'
#' ## `IRanges` to `tbl_df` ====
#' x <- as(IRanges, "tbl_df")
#' x <- as_tibble(IRanges)
#' print(x)
#'
#' ## `Matrix` to `DFrame` ====
#' from <- sparseMatrix
#' to <- as(from, "DFrame")
#' to
#'
#' ## `Matrix` to `data.frame` ====
#' x <- as(sparseMatrix, "data.frame")
#' head(x)
#'
#' ## `data.table` to `DFrame` ====
#' from <- data.table
#' to <- as(from, "DFrame")
#' head(to)
#'
#' ## `list` to `DFrame` ====
#' ## Use `as.DataFrame()` instead of `as()` for `list` class.
#' from <- list(
#'     a = list(c(1, 2), c(3, 4)),
#'     b = list(NULL, NULL)
#' )
#' to <- as.DataFrame(from)
#' to
#'
#' ## `tbl_df` to `DFrame` ====
#' from <- tbl_df
#' to <- as(from, "DFrame")
#' head(to)
NULL



## To DFrame ===================================================================

#' Coerce a `list` to `DFrame`
#'
#' @note Updated 2021-05-18.
#' @noRd
#'
#' @details
#' To store an object of a class that does not support coercion to `DFrame`,
#' wrap it in `I()`. The class must still have methods for `length` and `[`.
`as.DFrame,list` <-  # nolint
    function(x, row.names = NULL) {
        if (hasLength(x)) {
            assert(
                hasLength(x[[1L]]),
                msg = "First element of list is empty."
            )
            nc <- length(x)
            nr <- length(x[[1L]])
            x <- lapply(
                X = x,
                FUN = function(x) {
                    if (packageVersion("S4Vectors") >= 0.29) {
                        if (isAny(x = x, classes = c("List", "Rle"))) {
                            return(x)
                        }
                    }
                    I(x)
                }
            )
        } else {
            ## nocov start
            nc <- 0L
            nr <- 0L
            ## nocov end
        }
        args <- list(
            x,
            "row.names" = row.names,
            "check.names" = TRUE
        )
        out <- do.call(what = DataFrame, args = args)
        assert(identical(dim(out), c(nr, nc)))
        out
    }

## Updated 2021-02-19.
`as.DFrame,SimpleList` <-  # nolint
    `as.DFrame,list`

## Updated 2021-09-28.
`coerce,ANY,DFrame` <-  # nolint
    function(from) {
        to <- as.data.frame(from, stringsAsFactors = FALSE)
        to <- as(to, "DFrame")
        ## Move row names automatically, if defined.
        if (!hasRownames(to)) {
            rncol <- matchRownameColumn(to)
            if (is.character(rncol) && length(rncol) == 1L) {
                rownames(to) <- as.character(to[[rncol]])
                to[[rncol]] <- NULL
            }
        }
        to
    }

## Updated 2019-07-12.
`coerce,Matrix,DFrame` <-  # nolint
    `coerce,ANY,DFrame`

## Updated 2019-07-12.
`coerce,data.table,DFrame` <-  # nolint
    `coerce,ANY,DFrame`

## Updated 2019-07-12.
`coerce,tbl_df,DFrame` <-  # nolint
    `coerce,ANY,DFrame`



## To data.frame ===============================================================
## Updated 2019-07-20.
`as.data.frame,Matrix` <-  # nolint
    function(x, ...) {
        as.data.frame(as.matrix(x), ...)
    }

## Updated 2021-02-05.
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
            "start" = start(x),
            "end" = end(x),
            "width" = width(x),
            "row.names" = row.names,
            "check.rows" = TRUE,
            "check.names" = FALSE,
            "stringsAsFactors" = FALSE
        )
        mcols <- mcols(x, use.names = FALSE)
        if (!is.null(mcols)) {
            args[["mcols"]] <- as.data.frame(mcols)
        }
        do.call(what = data.frame, args = args)
    }

## Updated 2019-07-20.
`coerce,ANY,data.frame` <-  # nolint
    function(from) {
        as.data.frame(from)
    }

## Updated 2019-07-19.
`coerce,IRanges,data.frame` <-  # nolint
    `coerce,ANY,data.frame`

## Updated 2019-07-20.
`coerce,Matrix,data.frame` <-  # nolint
    `coerce,ANY,data.frame`



## To tibble ===================================================================

.tbl_rownames <-  # nolint
    quote(pkgconfig::get_config("tibble::rownames", "rowname"))

#' @rdname coerce
#' @export
## Updated 2021-09-28.
as_tibble.DFrame <-  # nolint
    function(x, ..., rownames) {
        ## FIXME Need to rename this.
        x <- `.coerce,DFrame,data.frame`(x)
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        as_tibble(x = x, ..., rownames = rownames)
    }

formals(as_tibble.DFrame)[["rownames"]] <- .tbl_rownames

#' @rdname coerce
#' @export
## Updated 2021-09-28.
as_tibble.DataFrame <-  # nolint
    as_tibble.DFrame

#' @rdname coerce
#' @export
## Updated 2020-01-19.
as_tibble.IRanges <-  # nolint
    function(x, ..., rownames) {
        x <- as(x, "data.frame")
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        as_tibble(x = x, ..., rownames = rownames)
    }

formals(as_tibble.IRanges)[["rownames"]] <- .tbl_rownames

#' @rdname coerce
#' @export
## Updated 2020-01-19.
as_tibble.GRanges <- as_tibble.IRanges  # nolint

## Updated 2019-07-19.
`coerce,ANY,tbl_df` <-  # nolint
    function(from) {
        as_tibble(from)
    }

## Updated 2019-07-19.
`coerce,data.frame,tbl_df` <-  # nolint
    `coerce,ANY,tbl_df`

## Updated 2019-07-19.
`coerce,DFrame,tbl_df` <-  # nolint
    `coerce,ANY,tbl_df`

## Updated 2019-07-20.
`coerce,GRanges,tbl_df` <-  # nolint
    `coerce,ANY,tbl_df`

## Updated 2020-01-19.
`coerce,IRanges,tbl_df` <-  # nolint
    `coerce,ANY,tbl_df`

rm(.tbl_rownames)



## To data.table ===============================================================

#' @rdname coerce
#' @export
## Updated 2021-09-28.
as.data.table.DFrame <-  # nolint
    function(x, keep.rownames = TRUE, ...) {  # nolint
        ## FIXME Need to rename this.
        x <- `.coerce,DFrame,data.frame`(x)
        if (!hasRownames(x)) {
            keep.rownames <- FALSE  # nolint
        }
        as.data.table(x = x, keep.rownames = keep.rownames, ...)
    }

#' @rdname coerce
#' @export
## Updated 2021-09-28.
as.data.table.DataFrame <-  # nolint
    as.data.table.DFrame

#' @rdname coerce
#' @export
## Updated 2020-01-19.
as.data.table.IRanges <-  # nolint
    function(x, keep.rownames = TRUE, ...) {  # nolint
        x <- as(x, "data.frame")
        if (!hasRownames(x)) {
            keep.rownames <- FALSE  # nolint
        }
        as.data.table(x = x, keep.rownames = keep.rownames, ...)
    }

#' @rdname coerce
#' @export
## Updated 2020-01-19.
as.data.table.GRanges <-  # nolint
    as.data.table.IRanges

## Updated 2019-07-19.
`coerce,ANY,data.table` <-  # nolint
    function(from) {
        as.data.table(from)
    }

## Updated 2019-07-19.
`coerce,data.frame,data.table` <-  # nolint
    `coerce,ANY,data.table`

## Updated 2019-07-19.
`coerce,DFrame,data.table` <-  # nolint
    `coerce,ANY,data.table`

## Updated 2020-01-19.
`coerce,IRanges,data.table` <-  # nolint
    `coerce,ANY,data.table`

## Updated 2019-07-20.
`coerce,GRanges,data.table` <-  # nolint
    `coerce,ANY,data.table`



## setMethod ===================================================================

#' @rdname coerce
#' @export
setMethod(
    f = "as.DFrame",
    signature = signature("SimpleList"),
    definition = `as.DFrame,SimpleList`
)

#' @rdname coerce
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature("SimpleList"),
    definition = `as.DFrame,SimpleList`
)

#' @rdname coerce
#' @export
setMethod(
    f = "as.DFrame",
    signature = signature("list"),
    definition = `as.DFrame,list`
)

#' @rdname coerce
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature("list"),
    definition = `as.DFrame,list`
)

#' @rdname coerce
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("IRanges"),
    definition = `as.data.frame,IRanges`
)

#' @rdname coerce
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("Matrix"),
    definition = `as.data.frame,Matrix`
)



## setAs =======================================================================

## Ensure these are redefined in basejump, for backward compatibility.

#' @rdname coerce
#' @name coerce,DFrame,data.table-method
setAs(
    from = "DFrame",
    to = "data.table",
    def = `coerce,DFrame,data.table`
)

#' @rdname coerce
#' @name coerce,DataFrame,data.table-method
setAs(
    from = "DataFrame",
    to = "data.table",
    def = `coerce,DFrame,data.table`
)

#' @rdname coerce
#' @name coerce,DFrame,tbl_df-method
setAs(
    from = "DFrame",
    to = "tbl_df",
    def = `coerce,DFrame,tbl_df`
)

#' @rdname coerce
#' @name coerce,DataFrame,tbl_df-method
setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = `coerce,DFrame,tbl_df`
)

#' @rdname coerce
#' @name coerce,GRanges,data.table-method
setAs(
    from = "GRanges",
    to = "data.table",
    def = `coerce,GRanges,data.table`
)

#' @rdname coerce
#' @name coerce,GRanges,tbl_df-method
setAs(
    from = "GRanges",
    to = "tbl_df",
    def = `coerce,GRanges,tbl_df`
)

#' @rdname coerce
#' @name coerce,IRanges,data.frame-method
setAs(
    from = "IRanges",
    to = "data.frame",
    def = `coerce,IRanges,data.frame`
)

#' @rdname coerce
#' @name coerce,IRanges,data.table-method
setAs(
    from = "IRanges",
    to = "data.table",
    def = `coerce,IRanges,data.table`
)

#' @rdname coerce
#' @name coerce,IRanges,tbl_df-method
setAs(
    from = "IRanges",
    to = "tbl_df",
    def = `coerce,IRanges,tbl_df`
)

#' @rdname coerce
#' @name coerce,Matrix,DFrame-method
setAs(
    from = "Matrix",
    to = "DFrame",
    def = `coerce,Matrix,DFrame`
)

#' @rdname coerce
#' @name coerce,Matrix,DataFrame-method
setAs(
    from = "Matrix",
    to = "DataFrame",
    def = `coerce,Matrix,DFrame`
)

#' @rdname coerce
#' @name coerce,Matrix,data.frame-method
setAs(
    from = "Matrix",
    to = "data.frame",
    def = `coerce,Matrix,data.frame`
)

#' @rdname coerce
#' @name coerce,data.frame,data.table-method
setAs(
    from = "data.frame",
    to = "data.table",
    def = `coerce,data.frame,data.table`
)

#' @rdname coerce
#' @name coerce,data.frame,tbl_df-method
setAs(
    from = "data.frame",
    to = "tbl_df",
    def = `coerce,data.frame,tbl_df`
)

#' @rdname coerce
#' @name coerce,data.table,DFrame-method
setAs(
    from = "data.table",
    to = "DFrame",
    def = `coerce,data.table,DFrame`
)

#' @rdname coerce
#' @name coerce,data.table,DataFrame-method
setAs(
    from = "data.table",
    to = "DataFrame",
    def = `coerce,data.table,DFrame`
)

#' @rdname coerce
#' @name coerce,tbl_df,DFrame-method
setAs(
    from = "tbl_df",
    to = "DFrame",
    def = `coerce,tbl_df,DFrame`
)

#' @rdname coerce
#' @name coerce,tbl_df,DataFrame-method
setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = `coerce,tbl_df,DFrame`
)
