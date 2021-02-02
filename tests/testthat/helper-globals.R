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

stopifnot(
    is(DFrame, "DataFrame"),
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
sparse <- sparseMatrix
tbl <- tbl_df

data(mtcars, package = "datasets", envir = environment())
mtcars <- as(mtcars, "DataFrame")

## nolint start
DataFrame <- S4Vectors::DataFrame
data.table <- data.table::data.table
hasInternet <- goalie::hasInternet
isSubset <- goalie::isSubset
mcols <- S4Vectors::mcols
metadata <- S4Vectors::metadata
seqnames <- GenomicRanges::seqnames
skip_on_docker <- goalie::skip_on_docker
tibble <- tibble::tibble
## nolint end
