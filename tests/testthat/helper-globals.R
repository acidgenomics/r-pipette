data(
    DFrame,
    GRanges,
    RangedSummarizedExperiment,
    SingleCellExperiment,
    SingleCellExperiment_Seurat,
    data.table,
    tbl_df,
    package = "acidtest",
    envir = environment()
)

df <- DFrame
dt <- data.table
gr <- GRanges
rse <- RangedSummarizedExperiment
sce <- SingleCellExperiment
sce_seurat <- SingleCellExperiment_Seurat  # nolint
tbl <- tbl_df

## nolint start
DataFrame <- S4Vectors::DataFrame
assay <- SummarizedExperiment::assay
assayNames <- SummarizedExperiment::assayNames
assays <- SummarizedExperiment::assays
`assays<-` <- SummarizedExperiment::`assays<-`
hasInternet <- goalie::hasInternet
mcols <- S4Vectors::mcols
metadata <- S4Vectors::metadata
rowRanges <- SummarizedExperiment::rowRanges
seqnames <- GenomicRanges::seqnames
skip_on_docker <- goalie::skip_on_docker
## nolint end

mat <- assay(rse)
sparse <- assay(sce)
