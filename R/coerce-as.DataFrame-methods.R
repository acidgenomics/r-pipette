#' @name as.DataFrame
#' @inherit AcidGenerics::as.DataFrame
#' @note Updated 2022-02-08.
#'
#' @examples
#' data(
#'     data.table,
#'     sparseMatrix,
#'     tbl_df,
#'     package = "AcidTest"
#' )
#'
#' ## `Matrix` to `DataFrame` ====
#' from <- sparseMatrix
#' to <- as.DataFrame(from)
#' print(to)
#'
#' ## `data.table` to `DataFrame` ====
#' from <- data.table
#' to <- as.DataFrame(from)
#' head(to)
#'
#' ## `list` to `DataFrame` ====
#' ## Use `as.DataFrame()` instead of `as()` for `list` class.
#' from <- list(
#'     "a" = list(c(1, 2), c(3, 4)),
#'     "b" = list(NULL, NULL)
#' )
#' to <- as.DataFrame(from)
#' print(to)
#'
#' ## `tbl_df` to `DataFrame` ====
#' from <- tbl_df
#' to <- as.DataFrame(from)
#' head(to)
NULL



## Updated 2022-02-07.
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



#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "GenomicRanges"),
    definition = `as.DataFrame,GenomicRanges`
)

#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "IntegerRanges"),
    definition = `as.DataFrame,IntegerRanges`
)

#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "SimpleList"),
    definition = `as.DataFrame,SimpleList`
)

#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "data.table"),
    definition = `as.DataFrame,data.table`
)

#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "list"),
    definition = `as.DataFrame,list`
)

#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "tbl_df"),
    definition = `as.DataFrame,tbl_df`
)
