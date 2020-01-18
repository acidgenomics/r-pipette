#' Coerce to data frame
#'
#' Coerce to `DataFrame`.
#'
#' @name coerce-DataFrame
#' @note Don't define `as` coercion method for `list` here. It will create
#'   issues with `data.frame` coercion. Use `as.DataFrame` instead to coerce
#'   a `list` to `DataFrame`.
#' @note Updated 2020-01-18.
#'
#' @inheritParams acidroxygen::params
#' @param row.names `NULL` or `character`.
#'
#' @return `DataFrame`.
#'
#' @examples
#' data(data.table, sparseMatrix, tbl_df, package = "acidtest")
#'
#' ## Matrix to DataFrame ====
#' from <- sparseMatrix
#' to <- as(from, "DataFrame")
#' to
#'
#' ## data.table to DataFrame ====
#' from <- data.table
#' to <- as(from, "DataFrame")
#' head(to)
#'
#' ## tbl_df to DataFrame ====
#' from <- tbl_df
#' to <- as(from, "DataFrame")
#' head(to)
#'
#' ## list to DataFrame ====
#' ## Use `as.DataFrame()` instead of `as()` for `list` class.
#' from <- list(
#'     a = list(c(1, 2), c(3, 4)),
#'     b = list(NULL, NULL)
#' )
#' to <- as.DataFrame(from)
#' to
NULL



#' @rdname coerce-DataFrame
#' @name as.DataFrame
#' @importFrom acidgenerics as.DataFrame
#' @usage as.DataFrame(x, ...)
#' @export
NULL



## Wrapping the columns in an `I()` works when passing to `DataFrame()`.
## See also `as_tibble()` for easy list to data frame coercion.
##
## `as()` method definition causes issues with `data.frame` to `DataFrame`
## coercion when defined, because data.frame inherits from list.
##
## Updated 2010-01-08.
`as.DataFrame,list` <-  # nolint
    function(x, row.names = NULL) {
        DataFrame(
            lapply(X = x, FUN = I),
            row.names = row.names
        )
    }



#' @rdname coerce-DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature("list"),
    definition = `as.DataFrame,list`
)



## Updated 2019-07-19.
`coerce,ANY,DataFrame` <-  # nolint
    function(from) {
        to <- as.data.frame(from, stringsAsFactors = FALSE)
        to <- as(to, "DataFrame")
        ## Move row names automatically, if defined.
        if (!hasRownames(to)) {
            rncol <- matchRowNameColumn(to)
            if (is.character(rncol) && length(rncol) == 1L) {
                rownames(to) <- as.character(to[[rncol]])
                to[[rncol]] <- NULL
            }
        }
        to
    }



## Updated 2019-07-12.
`coerce,Matrix,DataFrame` <-  # nolint
    `coerce,ANY,DataFrame`



#' @rdname coerce-DataFrame
#' @name coerce,Matrix,DataFrame-method
setAs(
    from = "Matrix",
    to = "DataFrame",
    def = `coerce,Matrix,DataFrame`
)



## Updated 2019-07-12.
`coerce,data.table,DataFrame` <-  # nolint
    `coerce,ANY,DataFrame`



#' @rdname coerce-DataFrame
#' @name coerce,data.table,DataFrame-method
setAs(
    from = "data.table",
    to = "DataFrame",
    def = `coerce,data.table,DataFrame`
)



## Updated 2019-07-12.
`coerce,tbl_df,DataFrame` <-  # nolint
    `coerce,ANY,DataFrame`



#' @rdname coerce-DataFrame
#' @name coerce,tbl_df,DataFrame-method
setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = `coerce,tbl_df,DataFrame`
)
