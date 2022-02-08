#' Coercion methods
#'
#' @name coerce
#' @note Updated 2022-02-08.
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
#' @return Modified object, of desired conversion class.
#'
#' @seealso
#' - `as.DataFrame()`.
#' - `as.data.table()`.
#' - `as_tibble()`.
#' - `getClass("DataFrame")`.
#' - `getClass("data.table")`.
#' - `getClass("tbl_df")`.
#'
#' @examples
#' data(
#'     DFrame,
#'     GRanges,
#'     IRanges,
#'     data.table,
#'     tbl_df,
#'     package = "AcidTest"
#' )
#'
#' ## `DataFrame` to `data.table` ====
#' from <- DFrame
#' to <- as(from, "data.table")
#' print(to)
#'
#' ## `DataFrame` to `tbl_df` ====
#' from <- DFrame
#' to <- as(from, "tbl_df")
#' print(to)
#'
#' ## `GenomicRanges` to `data.table` ====
#' from <- GRanges
#' to <- as(from, "data.table")
#' print(to)
#'
#' ## `GenomicRanges` to `tbl_df` ====
#' from <- GRanges
#' to <- as(from, "tbl_df")
#' print(to)
#'
#' ## `IRanges` to `data.table` ====
#' from <- IRanges
#' to <- as(from, "data.table")
#' print(to)
#'
#' ## `IRanges` to `tbl_df` ====
#' from <- IRanges
#' to <- as(from, "tbl_df")
#' print(to)
#'
#' ## `data.table` to `DataFrame` ====
#' from <- data.table
#' to <- as(from, "DataFrame")
#' head(to)
#'
#' ## `tbl_df` to `DataFrame` ====
#' from <- tbl_df
#' to <- as(from, "DataFrame")
#' head(to)
NULL



## Updated 2022-02-07.
`.coerce,ANY,DataFrame` <-  # nolint
    function(from) {
        as.DataFrame(from)
    }

## Updated 2022-02-07.
`.coerce,ANY,data.table` <-  # nolint
    function(from) {
        as.data.table(from)
    }

## Updated 2022-02-07.
`.coerce,ANY,tbl_df` <-  # nolint
    function(from) {
        as_tibble(from)
    }



## Updated 2019-07-19.
`coerce,DataFrame,data.table` <-  # nolint
    `.coerce,ANY,data.table`

## Updated 2021-10-14.
`coerce,DataFrame,tbl_df` <-  # nolint
    `.coerce,ANY,tbl_df`

## Updated 2021-10-14.
`coerce,GenomicRanges,data.table` <-  # nolint
    `.coerce,ANY,data.table`

## Updated 2021-10-14.
`coerce,GenomicRanges,tbl_df` <-  # nolint
    `.coerce,ANY,tbl_df`

## Updated 2021-10-14.
`coerce,IntegerRanges,data.table` <-  # nolint
    `.coerce,ANY,data.table`

## Updated 2021-10-14.
`coerce,IntegerRanges,tbl_df` <-  # nolint
    `.coerce,ANY,tbl_df`

## Internal only.
## Updated 2019-07-19.
`coerce,data.frame,data.table` <-  # nolint
    `.coerce,ANY,data.table`

## Internal only.
## Updated 2021-10-14.
`coerce,data.frame,tbl_df` <-  # nolint
    `.coerce,ANY,tbl_df`

## Updated 2021-10-14.
`coerce,data.table,DataFrame` <-  # nolint
    `.coerce,ANY,DataFrame`

## Updated 2021-10-14.
`coerce,tbl_df,DataFrame` <-  # nolint
    `.coerce,ANY,DataFrame`



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
