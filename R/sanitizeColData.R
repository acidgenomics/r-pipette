#' Sanitize column data
#'
#' Standardize data describing the colummns (e.g. samples or cells), keeping
#' only `atomic` columns.
#'
#' Always applied:
#'
#' - Non-`atomic` columns will be flattened to `atomic` if possible, and
#'   otherwise dropped.
#'
#' @section Sample data:
#'
#' The following conventions are enforced:
#'
#' - Required columns: `sampleName`.
#' - Blacklisted columns: `interestingGroups`, `sampleID`.
#' - All columns get coerced to `factor` and [`droplevels()`][base::droplevels]
#'   is applied.
#'
#' @param object `DataFrame`.
#' @export
#'
#' @return `DataFrame`.
#' Contains only `atomic` columns.
sanitizeColData <- function(object) {
    assert(
        is(object, "DataFrame"),
        isNonEmpty(object),
        hasRownames(object),
        hasColnames(object),
        hasValidDimnames(object)
    )
    atomize(object)
}
