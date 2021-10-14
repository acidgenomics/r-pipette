## This is needed to properly declare S4 `as()` coercion methods.
#' @name coerce
#' @export
#' @importFrom methods coerce
#' @exportMethod coerce
NULL



#' Coercion methods
#'
#' @name coerce
#' @note Updated 2021-10-14.
#'
#' @details
#' These conversion methods are primarily intended to interconvert between
#' popular tabular formats in R, including `data.frame`, `data.table`, `tbl_df`,
#' and the Bioconductor `DataFrame` classes.
#'
#' @section `DataFrame` (Bioconductor) coercion:
#'
#' Don't define `as()` coercion method for `list` here. It will create issues
#' with `data.frame` coercion. Use `as.DataFrame()` instead to coerce a `list`
#' to `DataFrame`.
#'
#' Wrapping the columns in an `I()` should work when passing to `DataFrame()`.
#' See also `as_tibble()` for easy list to data frame coercion.
#'
#' `as()` method definition causes issues with `data.frame` to `DataFrame`
#' coercion when defined, because `data.frame` inherits from list.
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
#' - `DataFrame` (from S4Vectors package).
#' - `GenomicRanges` (from GenomicRanges package).
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
#' - `DataFrame` (from S4Vectors package).
#' - `GenomicRanges` (from GenomicRanges package).
#'
#' ## S4 methods: `as()`
#'
#' Since `tbl_df` is a virtual class that extends `tbl` and `data.frame`, we
#' need to define an S4 coercion method that allows us to use `as()` to coerce
#' an object to a tibble.
#'
#' @inheritParams data.table::as.data.table
#' @inheritParams AcidRoxygen::params
#' @param row.names `NULL` or `character`.
#' @param ... Additional arguments.
#'
#' @return Modified object, of desired conversion class.
#'
#' @seealso
#' - `as.data.table()`.
#' - `as_tibble()`.
#' - `getClass("DataFrame")`.
#' - `getClass("data.table")`.
#' - `getClass("tbl_df")`.
#'
#' @examples
#' data(
#'     DataFrame,
#'     GenomicRanges,
#'     IntegerRanges,
#'     data.table,
#'     sparseMatrix,
#'     tibble,
#'     package = "AcidTest"
#' )
#'
#' ## `DataFrame` to `data.table` ====
#' from <- DataFrame
#' to <- as.data.table(from)
#' print(to)
#' to <- as(from, "data.table")
#' print(to)
#'
#' ## `DataFrame` to `tbl_df` (tibble) ====
#' from <- DataFrame
#' to <- as_tibble(from)
#' print(to)
#' to <- as(from, "tbl_df")
#' print(to)
#'
#' ## `GenomicRanges` to `data.table` ====
#' from <- GenomicRanges
#' to <- as.data.table(from)
#' print(to)
#' to <- as(from, "data.table")
#' print(to)
#'
#' ## `GenomicRanges` to `tbl_df` (tibble) ====
#' from <- GenomicRanges
#' to <- as_tibble(from)
#' print(to)
#' to <- as(from, "tbl_df")
#' print(to)
#'
#' ## `IntegerRanges` to `data.table` ====
#' from <- IntegerRanges
#' to <- as.data.table(from)
#' print(to)
#'
#' ## `IntegerRanges` to `tbl_df` (tibble) ====
#' from <- IntegerRanges
#' to <- as_tibble(from)
#' print(to)
#'
#' ## `data.table` to `DataFrame` ====
#' from <- data.table
#' to <- as.DataFrame(from)
#' head(to)
#' to <- as(from, "DataFrame")
#' head(to)
#'
#' ## `list` to `DataFrame` ====
#' ## Use `as.DataFrame()` instead of `as()` for `list` class.
#' from <- list(
#'     "a" = list(c(1, 2), c(3, 4)),
#'     "b" = list(NULL, NULL)
#' )
#' to <- as.DataFrame(from)
#' to
#'
#' ## `tbl_df` (tibble) to `DataFrame` ====
#' from <- tibble
#' to <- as.DataFrame(from)
#' head(to)
#' to <- as(from, "DataFrame")
#' head(to)
NULL



## To DataFrame ================================================================

#' Coerce a `list` to `DataFrame`
#'
#' @note Updated 2021-10-14.
#' @noRd
#'
#' @details
#' To store an object of a class that does not support coercion to `DataFrame`,
#' wrap it in `I()`. The class must still have methods for `length` and `[`.
`as.DataFrame,list` <-  # nolint
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
                    if (isAny(x = x, classes = c("List", "Rle"))) {
                        return(x)
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
`as.DataFrame,SimpleList` <-  # nolint
    `as.DataFrame,list`

## Updated 2021-10-14.
`as.DataFrame,data.table` <-  # nolint
    function(x) {
        to <- as.data.frame(x, stringsAsFactors = FALSE)
        to <- as(to, "DataFrame")
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

## Updated 2021-10-14.
`as.DataFrame,tbl_df` <-  # nolint
    `as.DataFrame,data.table`

## Updated 2021-10-14.
`coerce,data.table,DataFrame` <-  # nolint
    function(from) {
        as.DataFrame(from)
    }

## Updated 2021-10-14.
`coerce,tbl_df,DataFrame` <-  # nolint
    `coerce,data.table,DataFrame`



## To data.frame ===============================================================

## NOTE Ensure that methods are only used internally here.
## We want to avoid any unwanted collisions with other packages that define
## methods that dispatch onto standard data.frame class.

## Coerce an S4 DataFrame to a standard data.frame.
##
## This function will return an informative error if an S4 DataFrame contains
## complex columns that can't be coerced to atomic or list.
##
## Not exporting this method because we don't want to mask the default
## conventions currently used by Bioconductor.
##
## Updated 2021-10-14.
`.as.data.frame,DataFrame` <-  # nolint
    function(x) {
        ## Decode Rle columns, which can be coerced.
        x <- decode(x)
        ## Check for valid columns (atomic, list).
        valid <- vapply(
            X = x,
            FUN = function(x) {
                is.atomic(x) || is.list(x)
            },
            FUN.VALUE = logical(1L),
            USE.NAMES = TRUE
        )
        ## Error if S4 columns are nested.
        if (!all(valid)) {
            invalid <- x[, names(valid[!valid]), drop = FALSE]
            invalid <- vapply(
                X = invalid,
                FUN = class,
                FUN.VALUE = character(1L)
            )
            invalid <- sprintf("%s (%s)", names(invalid), invalid)
            abort(sprintf(
                fmt = paste(
                    "Only atomic and list columns are supported.",
                    "Invalid columns: %s.",
                    sep = "\n"
                ),
                toInlineString(invalid, n = 10L)
            ))
        }
        ## Don't use `as.data.frame()` here. It can unexpectedly sanitize row
        ## names (e.g. gene symbols), whereas the `as()` method does not.
        as(x, "data.frame")
    }

## Updated 2021-10-14.
`as.DataFrame,IntegerRanges` <-  # nolint
    function(x) {
        DataFrame(
            "start" = start(x),
            "end" = end(x),
            "width" = width(x),
            mcols(x, use.names = FALSE),
            row.names = names(x)
        )
    }

## Updated 2021-10-14.
`as.DataFrame,GenomicRanges` <-  # nolint
    function(x) {
        DataFrame(
            "seqnames" = seqnames(x),
            "start" = start(x),
            "end" = end(x),
            "width" = width(x),
            "strand" = strand(x),
            mcols(x, use.names = FALSE),
            row.names = names(x)
        )
    }



## To tibble ===================================================================

.tbl_rownames <-  # nolint
    quote(pkgconfig::get_config("tibble::rownames", "rowname"))

#' @rdname coerce
#' @export
## Updated 2021-10-14.
as_tibble.DataFrame <-  # nolint
    function(x, ..., rownames) {
        x <- `.as.data.frame,DataFrame`(x)
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        as_tibble(x = x, ..., rownames = rownames)
    }

formals(as_tibble.DataFrame)[["rownames"]] <- .tbl_rownames

#' @rdname coerce
#' @export
## Updated 2021-10-14.
as_tibble.GenomicRanges <-  # nolint
    function(x, ..., rownames) {
        x <- as.DataFrame(x)
        x <- as.data.frame(x)
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        as_tibble(x = x, ..., rownames = rownames)
    }

formals(as_tibble.GenomicRanges)[["rownames"]] <- .tbl_rownames

#' @rdname coerce
#' @export
## Updated 2021-10-14.
as_tibble.IntegerRanges <-  # nolint
    as_tibble.GenomicRanges

rm(.tbl_rownames)

## Updated 2021-10-14.
`.coerce,ANY,tbl_df` <-  # nolint
    function(from) {
        as_tibble(from)
    }

## Updated 2021-10-14.
`coerce,data.frame,tbl_df` <-  # nolint
    `.coerce,ANY,tbl_df`

## Updated 2021-10-14.
`coerce,DataFrame,tbl_df` <-  # nolint
    `.coerce,ANY,tbl_df`

## Updated 2021-10-14.
`coerce,GenomicRanges,tbl_df` <-  # nolint
    `.coerce,ANY,tbl_df`

## Updated 2021-10-14.
`coerce,IntegerRanges,tbl_df` <-  # nolint
    `.coerce,ANY,tbl_df`



## To data.table ===============================================================

#' @rdname coerce
#' @export
## Updated 2021-10-14.
as.data.table.DataFrame <-  # nolint
    function(
        x,
        keep.rownames = TRUE,  # nolint
        ...
    ) {
        x <- `.as.data.frame,DataFrame`(x)
        if (!hasRownames(x)) {
            keep.rownames <- FALSE  # nolint
        }
        as.data.table(x = x, keep.rownames = keep.rownames, ...)
    }

#' @rdname coerce
#' @export
## Updated 2021-10-14.
as.data.table.IntegerRanges <-  # nolint
    function(
        x,
        keep.rownames = TRUE,  # nolint
        ...
    ) {
        x <- as.DataFrame(x)
        x <- as.data.frame(x)
        if (!hasRownames(x)) {
            keep.rownames <- FALSE  # nolint
        }
        as.data.table(x = x, keep.rownames = keep.rownames, ...)
    }

#' @rdname coerce
#' @export
## Updated 2021-10-14.
as.data.table.GenomicRanges <-  # nolint
    as.data.table.IntegerRanges

## Updated 2021-10-14.
`.coerce,ANY,data.table` <-  # nolint
    function(from) {
        as.data.table(from)
    }

## Updated 2019-07-19.
`coerce,data.frame,data.table` <-  # nolint
    `.coerce,ANY,data.table`

## Updated 2019-07-19.
`coerce,DataFrame,data.table` <-  # nolint
    `.coerce,ANY,data.table`

## Updated 2021-10-14.
`coerce,IntegerRanges,data.table` <-  # nolint
    `.coerce,ANY,data.table`

## Updated 2021-10-14.
`coerce,GenomicRanges,data.table` <-  # nolint
    `.coerce,ANY,data.table`



## setMethod ===================================================================

#' @rdname coerce
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "GenomicRanges"),
    definition = `as.DataFrame,GenomicRanges`
)

#' @rdname coerce
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "IntegerRanges"),
    definition = `as.DataFrame,IntegerRanges`
)

#' @rdname coerce
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "SimpleList"),
    definition = `as.DataFrame,SimpleList`
)

#' @rdname coerce
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "data.table"),
    definition = `as.DataFrame,data.table`
)

#' @rdname coerce
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "list"),
    definition = `as.DataFrame,list`
)

#' @rdname coerce
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "tbl_df"),
    definition = `as.DataFrame,tbl_df`
)



## setAs =======================================================================

## Ensure these are redefined in basejump, for backward compatibility.

#' @rdname coerce
#' @name coerce,DataFrame,data.table-method
setAs(
    from = "DataFrame",
    to = "data.table",
    def = `coerce,DataFrame,data.table`
)

#' @rdname coerce
#' @name coerce,DataFrame,tbl_df-method
setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = `coerce,DataFrame,tbl_df`
)

#' @rdname coerce
#' @name coerce,GenomicRanges,data.table-method
setAs(
    from = "GenomicRanges",
    to = "data.table",
    def = `coerce,GenomicRanges,data.table`
)

#' @rdname coerce
#' @name coerce,GenomicRanges,tbl_df-method
setAs(
    from = "GenomicRanges",
    to = "tbl_df",
    def = `coerce,GenomicRanges,tbl_df`
)

#' @rdname coerce
#' @name coerce,IntegerRanges,data.table-method
setAs(
    from = "IntegerRanges",
    to = "data.table",
    def = `coerce,IntegerRanges,data.table`
)

#' @rdname coerce
#' @name coerce,IntegerRanges,tbl_df-method
setAs(
    from = "IntegerRanges",
    to = "tbl_df",
    def = `coerce,IntegerRanges,tbl_df`
)

#' @rdname coerce
#' @name coerce,data.table,DataFrame-method
setAs(
    from = "data.table",
    to = "DataFrame",
    def = `coerce,data.table,DataFrame`
)

#' @rdname coerce
#' @name coerce,tbl_df,DataFrame-method
setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = `coerce,tbl_df,DataFrame`
)
