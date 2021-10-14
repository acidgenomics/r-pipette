## nolint start

options(
    "acid.export.engine" = NULL,
    "acid.import.engine" = NULL
)

data(
    DFrame,
    GRanges,
    IRanges,
    data.table,
    matrix,
    sparseMatrix,
    tbl_df,
    package = "AcidTest",
    envir = environment()
)
data(
    mtcars,
    package = "datasets",
    envir = environment()
)
stopifnot(
    is(DFrame, "DFrame"),
    is(GRanges, "GRanges"),
    is(IRanges, "IRanges"),
    is(data.table, "data.table"),
    is(sparseMatrix, "sparseMatrix"),
    is(tbl_df, "tbl_df")
)

df <- DFrame
dt <- data.table
gr <- GRanges
ir <- IRanges
mat <- matrix
mtcars <- as(mtcars, "DFrame")
sparse <- sparseMatrix
tbl <- tbl_df

DataFrame <- AcidGenerics::DataFrame
data.table <- data.table::data.table
hasInternet <- goalie::hasInternet
isSubset <- goalie::isSubset
mcols <- AcidGenerics::mcols
metadata <- AcidGenerics::metadata
seqnames <- GenomicRanges::seqnames
skip_on_docker <- goalie::skip_on_docker
tibble <- tibble::tibble

## nolint end
