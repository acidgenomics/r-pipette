context("atomize")

test_that("DataFrame", {
    data <- rowData(rse)
    # Note that older versions of SummarizedExperiment (e.g. 3.6) don't assign
    # rownames correctly to `rowData`, so we're fixing this here.
    rownames(data) <- rownames(rse)
    object <- atomize(data)
    expect_s4_class(object, "DataFrame")
    expect_true(hasRownames(object))
    expect_identical(
        object = lapply(object, class),
        expected = list(
            geneID = "character",
            geneName = "factor",
            geneBiotype = "factor",
            broadClass = "factor"
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
            geneID = "character",
            geneName = "factor",
            geneBiotype = "factor",
            broadClass = "factor"
        )
    )
})
