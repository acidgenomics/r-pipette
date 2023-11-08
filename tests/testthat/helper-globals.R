## nolint start

options(
    "acid.export.engine" = NULL,
    "acid.import.engine" = NULL
)

data <- utils::data

data(
    DFrame,
    GRanges,
    IRanges,
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
    is(DFrame, "DFrame"),
    is(GRanges, "GRanges"),
    is(IRanges, "IRanges"),
    is(sparseMatrix, "sparseMatrix")
)

df <- DFrame
gr <- GRanges
ir <- IRanges
mat <- matrix
mtcars <- as(mtcars, "DFrame")
sparse <- sparseMatrix

engines <- c("base", "data.table", "readr")

DataFrame <- S4Vectors::DataFrame
GRanges <- GenomicRanges::GRanges
List <- S4Vectors::List
hasInternet <- goalie::hasInternet
isAnExistingUrl <- goalie::isAnExistingUrl
isSubset <- goalie::isSubset
isWindows <- goalie::isWindows
mcols <- S4Vectors::mcols
metadata <- S4Vectors::metadata
seqnames <- GenomicRanges::seqnames
tempdir2 <- AcidBase::tempdir2
unlink2 <- AcidBase::unlink2
with_collate <- withr::with_collate

## nolint end
