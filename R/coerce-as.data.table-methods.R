#' Coerce object to a data.table
#'
#' @name as.data.table
#' @note Updated 2022-02-08.
#'
#' @details
#' Our defined methods attempt to improve on the defaults in the data.table
#' package to ensure that row names are not dropped by default, which is a poor
#' default for bioinformatics. This is accomplished by setting
#' `keep.rownames = "rowname"` by default instead of `keep.rownames = NULL`.
#' Note that we're manually defining the `"rowname"` column instead of using
#' `TRUE`, to match the conventions used in our `as_tibble()` methods.
#'
#' The package extends `as.data.table()` method support for these S4 classes:
#'
#' - `DataFrame` (from S4Vectors package).
#' - `GenomicRanges` (from GenomicRanges package).
#'
#' @return `data.table`.
#'
#' @seealso
#' - `data.table::as.data.table()`.
#'
#' @examples
#' data(
#'     DFrame,
#'     GRanges,
#'     IRanges,
#'     package = "AcidTest"
#' )
#'
#' ## `DataFrame` to `data.table` ====
#' from <- DFrame
#' to <- as.data.table(from)
#' print(to)
#'
#' ## `GenomicRanges` to `data.table` ====
#' from <- GRanges
#' to <- as.data.table(from)
#' print(to)
#'
#' ## `IRanges` to `data.table` ====
#' from <- IRanges
#' to <- as.data.table(from)
#' print(to)
NULL



#' @rdname as.data.table
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

#' @rdname as.data.table
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

#' @rdname as.data.table
#' @export
## Updated 2021-10-14.
as.data.table.GenomicRanges <-  # nolint
    as.data.table.IntegerRanges
