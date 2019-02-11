context("Sanitize")

load(system.file("extdata", "rse.rda", package = "brio"))



# atomize ======================================================================
test_that("atomize : DataFrame", {
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

test_that("atomize : GRanges", {
    object <- atomize(rowRanges(rse))
    expect_s4_class(object, "GRanges")
    expect_true(hasNames(object))
    expect_identical(
        object = lapply(S4Vectors::mcols(object), class),
        expected = list(
            geneID = "character",
            geneName = "factor",
            geneBiotype = "factor",
            broadClass = "factor"
        )
    )
})
