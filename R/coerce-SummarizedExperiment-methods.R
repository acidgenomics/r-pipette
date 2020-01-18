#' Coerce to summarized experiment
#'
#' Coerce to `SummarizedExperiment`.
#'
#' Improved S4 methods for reliably coercing objects that extend
#' `RangedSummarizedExperiment` to a standard `SummarizedExperiment`, that
#' doesn't drop [`rowData()`][SummarizedExperiment::rowData].
#'
#' Related S4 coercion method of interest:
#'
#' ```
#' getMethod(
#'     f = "coerce",
#'     signature = signature(
#'         from = "RangedSummarizedExperiment",
#'         to = "SummarizedExperiment"
#'     ),
#'     where = asNamespace("SummarizedExperiment")
#' )
#' ```
#'
#' @name coerce-SummarizedExperiment
#' @note Updated 2020-01-18.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @return `SummarizedExperiment`.
#'
#' @examples
#' suppressPackageStartupMessages(library(SummarizedExperiment))
#' data(RangedSummarizedExperiment, package = "acidtest")
#'
#' ## RangedSummarizedExperiment ===
#' ## Current S4 coercion drops row metadata.
#' names(metadata(rowRanges(RangedSummarizedExperiment)))
#' se <- as(RangedSummarizedExperiment, "SummarizedExperiment")
#' names(metadata(rowData(se)))
#' ## Our alternate S3 method preserves the metadata.
#' se <- as.SummarizedExperiment(RangedSummarizedExperiment)
#' names(metadata(rowData(se)))
NULL



#' @rdname coerce-SummarizedExperiment
#' @name as.SummarizedExperiment
#' @importFrom acidgenerics as.SummarizedExperiment
#' @usage as.SummarizedExperiment(x, ...)
#' @export
NULL



## Updated 2019-08-23.
`as.SummarizedExperiment,SummarizedExperiment` <-  # nolint
    function(x) {
        rowMeta <- metadata(rowData(x))
        x <- as(x, "SummarizedExperiment")
        metadata(rowData(x)) <- rowMeta
        x
    }



#' @rdname coerce-SummarizedExperiment
#' @export
setMethod(
    f = "as.SummarizedExperiment",
    signature = signature("SummarizedExperiment"),
    definition = `as.SummarizedExperiment,SummarizedExperiment`
)



## Updated 2019-08-23.
`as.SummarizedExperiment,RangedSummarizedExperiment` <-  # nolint
    function(x) {
        rowMeta <- metadata(rowRanges(x))
        x <- as(x, "RangedSummarizedExperiment")
        x <- as(x, "SummarizedExperiment")
        metadata(rowData(x)) <- rowMeta
        x
    }



#' @rdname coerce-SummarizedExperiment
#' @export
setMethod(
    f = "as.SummarizedExperiment",
    signature = signature("RangedSummarizedExperiment"),
    definition = `as.SummarizedExperiment,RangedSummarizedExperiment`
)
