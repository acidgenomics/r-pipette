#' Sanitize row data
#'
#' Coerce row annotations (e.g. genes or transcripts) to `DataFrame`, and keep
#' only `atomic` columns. Complex columns (e.g. Entrez ID `list`) will fail to
#' write to disk as CSVs.
#'
#' @export
#'
#' @param object `DataFrame` or `GRanges`.
#'
#' @return `DataFrame`.
#' Contains only `atomic` columns.
sanitizeRowData <- function(object) {
    assert(
        is(object, "DataFrame"),
        hasRownames(object)
    )
    atomize(object)
}



#' @rdname sanitizeRowData
#' @export
sanitizeRowRanges <- function(object) {
    assert(
        is(object, "GRanges"),
        hasNames(object)
    )
    atomize(object)
}
