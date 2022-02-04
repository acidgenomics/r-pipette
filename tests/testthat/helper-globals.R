## nolint start

options(
    "acid.export.engine" = NULL,
    "acid.import.engine" = NULL
)

data(
    DataFrame,
    GenomicRanges,
    IntegerRanges,
    data.table,
    matrix,
    sparseMatrix,
    tibble,
    package = "AcidTest",
    envir = environment()
)
data(
    mtcars,
    package = "datasets",
    envir = environment()
)
stopifnot(
    is(DataFrame, "DataFrame"),
    is(GenomicRanges, "GenomicRanges"),
    is(IntegerRanges, "IntegerRanges"),
    is(data.table, "data.table"),
    is(sparseMatrix, "sparseMatrix"),
    is(tibble, "tbl_df")
)

df <- DataFrame
dt <- data.table
gr <- GenomicRanges
ir <- IntegerRanges
mat <- matrix
mtcars <- as(mtcars, "DataFrame")
sparse <- sparseMatrix
tbl <- tibble

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
