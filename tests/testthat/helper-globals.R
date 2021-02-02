## FIXME MAKE THIS LIGHTER...

data(
    DFrame,
    GRanges,
    IRanges,
    RangedSummarizedExperiment,
    SingleCellExperiment,
    SingleCellExperiment_Seurat,
    data.table,
    sparseMatrix,
    tbl_df,
    package = "AcidTest",
    envir = environment()
)

stopifnot(
    is(DFrame, "DataFrame"),
    is(GRanges, "GRanges"),
    is(IRanges, "IRanges"),
    is(RangedSummarizedExperiment, "RangedSummarizedExperiment"),
    is(data.table, "data.table"),
    is(sparseMatrix, "sparseMatrix"),
    is(tbl_df, "tbl_df")
)

df <- DFrame
dt <- data.table
gr <- GRanges
ir <- IRanges
rse <- RangedSummarizedExperiment
sce <- SingleCellExperiment
sce_seurat <- SingleCellExperiment_Seurat  # nolint
sparse <- sparseMatrix
tbl <- tbl_df

data(mtcars, package = "datasets", envir = environment())
mtcars <- as(mtcars, "DataFrame")

## nolint start
DataFrame <- S4Vectors::DataFrame
assay <- SummarizedExperiment::assay
assayNames <- SummarizedExperiment::assayNames
assays <- SummarizedExperiment::assays
`assays<-` <- SummarizedExperiment::`assays<-`
colData <- SummarizedExperiment::colData
data.table <- data.table::data.table
hasInternet <- goalie::hasInternet
isSubset <- goalie::isSubset
mcols <- S4Vectors::mcols
metadata <- S4Vectors::metadata
rowData <- SummarizedExperiment::rowData
rowRanges <- SummarizedExperiment::rowRanges
seqnames <- GenomicRanges::seqnames
skip_on_docker <- goalie::skip_on_docker
tibble <- tibble::tibble
## nolint end

mat <- assay(rse)
sparse <- assay(sce)
