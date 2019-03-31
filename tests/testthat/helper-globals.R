data(
    df, gr, rse, sce,
    package = "acidtest",
    envir = environment()
)

# nolint start
DataFrame <- S4Vectors::DataFrame
assay <- SummarizedExperiment::assay
rowRanges <- SummarizedExperiment::rowRanges
mcols <- S4Vectors::mcols
metadata <- S4Vectors::metadata
seqnames <- GenomicRanges::seqnames
tibble <- tibble::tibble
# nolint end

mat <- assay(rse)
sparse <- assay(sce)
