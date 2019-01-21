context("Sanitize")

# FIXME This approach results in build check failure.
load(system.file("extdata", "rse.rda", package = "brio"))



# sanitizeRowData ==============================================================
test_that("sanitizeRowData", {
    object <- sanitizeRowData(rowData(rse))
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

test_that("sanitizeRowRanges", {
    object <- sanitizeRowRanges(rowRanges(rse))
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
