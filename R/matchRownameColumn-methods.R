#' Match row name column
#'
#' Automatically detect row names column, if defined.
#'
#' The data.table package uses "rn" by default, whereas tibble uses "rowname".
#'
#' @name matchRownameColumn
#' @note Updated 2021-10-14.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param choices `character`.
#' Column name choices to use internally for matching.
#' Note that case-insensitive matching is performed against `make.names()`
#' return internally. Either dots (".") or underscores ("_") used as word
#' separators will match.
#'
#' @return `character(1)` or `NULL`.
#'
#' - data.table: `"rn"`.
#' - tibble: `"rowname"`.
#'
#' @examples
#' ## data.table ====
#' if (goalie::isInstalled("data.table")) {
#'     data(data.table, package = "AcidTest")
#'     object <- data.table
#'     matchRownameColumn(object)
#' }
#'
#' ## tbl_df ====
#' if (goalie::isInstalled("tibble")) {
#'     data(tibble, package = "AcidTest")
#'     object <- tibble
#'     matchRownameColumn(object)
#' }
NULL



## Updated 2023-04-26.
`matchRownameColumn,DFrame` <- # nolint
    function(object,
             choices = c(
                 "rn",
                 "row.name",
                 "row.names",
                 "rowname",
                 "rownames"
             )) {
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
            abort(sprintf(
                "Multiple row names columns detected: %s.",
                toInlineString(fail, n = 10L)
            ))
        }
    }



## Updated 2021-10-14.
`matchRownameColumn,data.frame` <- # nolint
    `matchRownameColumn,DFrame`



#' @rdname matchRownameColumn
#' @export
setMethod(
    f = "matchRownameColumn",
    signature = signature(object = "DFrame"),
    definition = `matchRownameColumn,DFrame`
)

#' @rdname matchRownameColumn
#' @export
setMethod(
    f = "matchRownameColumn",
    signature = signature(object = "data.frame"),
    definition = `matchRownameColumn,data.frame`
)
