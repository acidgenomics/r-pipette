## nolint start

options(
    "acid.export.engine" = NULL,
    "acid.import.engine" = NULL
)

data(
    DFrame,  # FIXME
    GRanges,  # FIXME
    IRanges,  # FIXME
    data.table,
    matrix,
    sparseMatrix,
    tbl_df,  # FIXME
    package = "AcidTest",
    envir = environment()
)
data(
    mtcars,
    package = "datasets",
    envir = environment()
)
stopifnot(
    is(DFrame, "DataFrame"),  # FIXME
    is(GRanges, "GRanges"),  # FIXME
    is(IRanges, "IRanges"),  # FIXME
    is(data.table, "data.table"),
    is(sparseMatrix, "sparseMatrix"),
    is(tbl_df, "tbl_df")  # FIXME
)

df <- DFrame  # FIXME
dt <- data.table
gr <- GRanges  # FIXME
ir <- IRanges  # FIXME
mat <- matrix
mtcars <- as(mtcars, "DataFrame")
sparse <- sparseMatrix
tbl <- tbl_df  # FIXME

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
