#' Match row name column
#'
#' Automatically detect row names column, if defined.
#'
#' The data.table package uses "rn" by default, whereas tibble uses "rowname".
#'
#' @name matchRowNameColumn
#' @note Updated 2020-01-18.
#'
#' @inheritParams AcidRoxygen::params
#' @param choices `character`.
#'   Column name choices to use internally for matching.
#'   Note that case-insensitive matching is performed against [`make.names()`]
#'   return internally. Either dots (".") or underscores ("_") used as word
#'   separators will match.
#' @param ... Additional arguments.
#'
#' @return `character(1)` or `NULL`.
#'
#'   - data.table: `"rn"`.
#'   - tibble: `"rowname"`.
#'
#' @examples
#' data(data.table, tbl_df, package = "AcidTest")
#'
#' ## data.table ====
#' matchRowNameColumn(data.table)
#'
#' ## tbl_df ====
#' matchRowNameColumn(tbl_df)
NULL



## Updated 2020-08-11.
`matchRowNameColumn,data.frame` <-  # nolint
    function(
        object,
        choices = c(
            "rn",
            "row.name",
            "row.names",
            "rowname",
            "rownames"
        )
    ) {
        assert(!hasRownames(object))
        match <- na.omit(match(
            x = choices,
            table = make.names(tolower(colnames(object))),
            nomatch = NA_integer_
        ))
        if (!hasLength(match)) {
            NULL
        } else if (length(match) == 1L) {
            col <- colnames(object)[[match]]
            rownames <- as.character(object[[col]])
            assert(validNames(rownames))
            col
        } else if (length(match) > 1L) {
            fail <- colnames(object)[match]
            stop(sprintf(
                "Multiple row names columns detected: %s.",
                toString(fail, width = 100L)
            ))
        }
    }



#' @rdname matchRowNameColumn
#' @export
setMethod(
    f = "matchRowNameColumn",
    signature = signature("data.frame"),
    definition = `matchRowNameColumn,data.frame`
)



`matchRowNameColumn,DataFrame` <- `matchRowNameColumn,data.frame`  # nolint



#' @rdname matchRowNameColumn
#' @export
setMethod(
    f = "matchRowNameColumn",
    signature = signature("DataFrame"),
    definition = `matchRowNameColumn,DataFrame`
)
