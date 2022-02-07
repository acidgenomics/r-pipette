#' Coerce an object to a tibble
#'
#' @rdname coerce-as_tibble
#' @name as_tibble
#' @note Updated 2022-02-07.
#'
#' @inheritParams AcidRoxygen::params
#' @param rownames
#'   How to treat existing row names.
#'   Refer to `tibble::as_tibble` for current options.
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
#' ## `DataFrame` to `tbl_df` (tibble) ====
#' from <- DataFrame
#' to <- as_tibble(from)
#' print(to)
#'
#' ## `GenomicRanges` to `tbl_df` (tibble) ====
#' from <- GenomicRanges
#' to <- as_tibble(from)
#' print(to)
#'
#' ## `IntegerRanges` to `tbl_df` (tibble) ====
#' from <- IntegerRanges
#' to <- as_tibble(from)
#' print(to)
NULL



.tbl_rownames <-  # nolint
    quote(pkgconfig::get_config("tibble::rownames", "rowname"))

#' @rdname coerce-as_tibble
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

#' @rdname coerce-as_tibble
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

#' @rdname coerce-as_tibble
#' @export
## Updated 2021-10-14.
as_tibble.IntegerRanges <-  # nolint
    as_tibble.GenomicRanges

rm(.tbl_rownames)
