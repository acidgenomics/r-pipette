data(
    df, dt, gr, rse, sce, tbl,
    package = "acidtest",
    envir = environment()
)

## nolint start
DataFrame <- S4Vectors::DataFrame
assay <- SummarizedExperiment::assay
assayNames <- SummarizedExperiment::assayNames
`assays<-` <- SummarizedExperiment::`assays<-`
rowRanges <- SummarizedExperiment::rowRanges
mcols <- S4Vectors::mcols
metadata <- S4Vectors::metadata
seqnames <- GenomicRanges::seqnames
skip_on_docker <- goalie::skip_on_docker
tibble <- tibble::tibble
## nolint end

mat <- assay(rse)
sparse <- assay(sce)
