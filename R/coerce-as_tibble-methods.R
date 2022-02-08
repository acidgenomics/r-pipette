#' Coerce an object to a tibble
#'
#' @name as_tibble
#' @note Updated 2022-02-08.
#'
#' @details
#' Our defined methods attempt to improve on the defaults in the tibble package
#' to ensure that row names are not dropped by default, which is a poor default
#' for bioinformatics. This is accomplished by setting `rownames = "rowname"` by
#' default instead of `rownames = NULL`.
#'
#' Note that we're matching `as_tibble()` convention here, using `rowname` as
#' column for row names assignment. We also using similar internal assert checks
#' here, allowing atomic and/or list columns only.
#'
#' The package extends `as_tibble()` method support for these S4 classes:
#'
#' - `DataFrame` (from S4Vectors package).
#' - `GenomicRanges` (from GenomicRanges package).
#'
#' @inheritParams AcidRoxygen::params
#' @param rownames
#'   How to treat existing row names.
#'   Refer to `tibble::as_tibble` for current options.
#'
#' @return `tbl_df` (tibble).
#'
#' @seealso
#' - `tibble::as_tibble()`.
#'
#' @examples
#' data(
#'     DataFrame,
#'     GenomicRanges,
#'     IntegerRanges,
#'     package = "AcidTest"
#' )
#'
#' ## `DataFrame` to `tbl_df` ====
#' from <- DataFrame
#' to <- as_tibble(from)
#' print(to)
#'
#' ## `GenomicRanges` to `tbl_df` ====
#' from <- GenomicRanges
#' to <- as_tibble(from)
#' print(to)
#'
#' ## `IntegerRanges` to `tbl_df` ====
#' from <- IntegerRanges
#' to <- as_tibble(from)
#' print(to)
NULL



.tbl_rownames <-  # nolint
    quote(pkgconfig::get_config("tibble::rownames", "rowname"))

#' @rdname as_tibble
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

#' @rdname as_tibble
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

#' @rdname as_tibble
#' @export
## Updated 2021-10-14.
as_tibble.IntegerRanges <-  # nolint
    as_tibble.GenomicRanges

rm(.tbl_rownames)
