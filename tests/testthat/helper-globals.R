## nolint start

options(
    "acid.export.engine" = NULL,
    "acid.import.engine" = NULL
)

data(
    DataFrame,
    GenomicRanges,
    IntegerRanges,
    matrix,
    sparseMatrix,
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
    is(sparseMatrix, "sparseMatrix")
)

df <- DataFrame
gr <- GenomicRanges
ir <- IntegerRanges
mat <- matrix
mtcars <- as(mtcars, "DataFrame")
sparse <- sparseMatrix

DataFrame <- S4Vectors::DataFrame
GRanges <- GenomicRanges::GRanges
hasInternet <- goalie::hasInternet
isSubset <- goalie::isSubset
mcols <- S4Vectors::mcols
metadata <- S4Vectors::metadata
seqnames <- GenomicRanges::seqnames

## nolint end
