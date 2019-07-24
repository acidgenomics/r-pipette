data(
    DataFrame,
    GRanges,
    RangedSummarizedExperiment,
    SingleCellExperiment,
    data.table,
    tbl_df,
    package = "acidtest",
    envir = environment()
)

df <- DataFrame
dt <- data.table
gr <- GRanges
rse <- RangedSummarizedExperiment
sce <- SingleCellExperiment
tbl <- tbl_df

## nolint start
DataFrame <- S4Vectors::DataFrame
assay <- SummarizedExperiment::assay
assayNames <- SummarizedExperiment::assayNames
`assays<-` <- SummarizedExperiment::`assays<-`
hasInternet <- goalie::hasInternet
mcols <- S4Vectors::mcols
metadata <- S4Vectors::metadata
rowRanges <- SummarizedExperiment::rowRanges
seqnames <- GenomicRanges::seqnames
skip_on_docker <- goalie::skip_on_docker
tibble <- tibble::tibble
## nolint end

mat <- assay(rse)
sparse <- assay(sce)
