context("atomize")

test_that("DataFrame", {
    data <- rowData(rse)
    rownames(data) <- rownames(rse)
    object <- atomize(data)
    expect_s4_class(object, "DataFrame")
    expect_true(hasRownames(object))
    expect_identical(
        object = lapply(object, class),
        expected = list(
            broadClass = "factor",
            description = "character",
            geneBiotype = "character",
            geneID = "character",
            geneIDVersion = "character",
            geneName = "character",
            seqCoordSystem = "character"
        )
    )
})

test_that("GRanges", {
    object <- atomize(rowRanges(rse))
    expect_s4_class(object, "GRanges")
    expect_true(hasNames(object))
    expect_identical(
        object = lapply(mcols(object), class),
        expected = list(
            broadClass = "factor",
            description = "character",
            geneBiotype = "character",
            geneID = "character",
            geneIDVersion = "character",
            geneName = "character",
            seqCoordSystem = "character"
        )
    )
})
