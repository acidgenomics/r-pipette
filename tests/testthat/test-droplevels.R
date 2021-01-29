context("droplevels")

Rle <- structure("Rle", package = "S4Vectors")  # nolint

test_that("DataFrame", {
    cd <- colData(rse)
    expect_s4_class(cd, "DataFrame")
    x <- droplevels(cd)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        object = lapply(x, class),
        expected = list(condition = "factor")
    )
})

test_that("Early return on empty DataFrame", {
    expect_identical(droplevels(DataFrame()), DataFrame())
})

test_that("IRanges", {
    expect_identical(droplevels(ir), ir)
})

test_that("GRanges", {
    gr <- rowRanges(rse)
    names(mcols(gr)) <- camelCase(names(mcols(gr)), strict = TRUE)
    expect_s4_class(gr, "GRanges")
    x <- droplevels(gr)
    expect_s4_class(x, "GRanges")
    if (packageVersion("GenomicRanges") < "1.31") {
        AsIs <- "AsIs"  # nolint
    } else {
        AsIs <- "list"  # nolint
    }
    expect_identical(
        object = lapply(mcols(x), class),
        expected = list(
            "broadClass" = Rle,
            "description" = Rle,
            "entrezId" = AsIs,
            "geneBiotype" = Rle,
            "geneId" = Rle,
            "geneIdVersion" = Rle,
            "geneName" = Rle,
            "seqCoordSystem" = Rle
        )
    )
})

test_that("SummarizedExperiment", {
    x <- droplevels(rse)
    expect_s4_class(x, "RangedSummarizedExperiment")
    ## Check for factor columns.
    ok <- any(vapply(
        X = decode(rowData(x)),
        FUN = is.factor,
        FUN.VALUE = logical(1L)
    ))
    expect_true(ok)
    ok <- any(vapply(
        X = decode(colData(x)),
        FUN = is.factor,
        FUN.VALUE = logical(1L)
    ))
    expect_true(ok)
})
