context("Sanitize")

load(system.file("extdata", "rse.rda", package = "brio"))



# sanitizeRowData ==============================================================
test_that("sanitizeRowData", {
    object <- sanitizeRowData(rowRanges(rse))
    expect_s4_class(object, "GRanges")
    df <- as.data.frame(object)
    expect_true(hasRownames(df))
    expect_identical(
        object = lapply(df, class),
        expected = list(
            seqnames = "factor",
            start = "integer",
            end = "integer",
            width = "integer",
            strand = "factor",
            geneID = "character",
            geneName = "factor",
            geneBiotype = "factor",
            broadClass = "factor"
        )
    )
})
