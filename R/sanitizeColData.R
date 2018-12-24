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
#'
#' @examples
#' data(rse, package = "basejump")
#' from <- colData(rse)
#' print(from)
#' to <- sanitizeColData(from)
#' all(vapply(to, is.factor, logical(1L)))
#' print(to)
sanitizeColData <- function(object) {
    assert(
        is(object, "DataFrame"),
        isNonEmpty(object),
        hasRownames(object),
        hasColnames(object),
        hasValidDimnames(object)
    )
    .atomize(object)
}



# Consider exporting this.
.atomize <- function(object) {
    # First, coerce to S3 data frame.
    # This step helps coerce nested S4 data to atomic columns.
    # This will also decode Rle columns.
    object <- as.data.frame(object)
    # Keep only atomic columns. Complex columns won't write to disk as CSVs
    # or work with R Markdown functions.
    keep <- vapply(X = object, FUN = is.atomic, FUN.VALUE = logical(1L))
    object <- object[, keep, drop = FALSE]
    assert(hasLength(object))
    as(object, "DataFrame")
}



# Consider exporting this.
# See `encode()` for Rle approach.
.factorize <- function(object) {
    out <- lapply(
        X = object,
        FUN = function(x) {
            droplevels(as.factor(x))
        }
    )
    out <- as(out, class(object)[[1L]])
    names(out) <- names(object)
    rownames <- rownames(object)
    if (!is.null(rownames)) {
        rownames(out) <- rownames
    }
    out
}
