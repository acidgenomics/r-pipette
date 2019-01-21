context("Sanitize")

# FIXME This approach results in build check failure.
load(system.file("extdata", "rse.rda", package = "brio"))



# sanitizeRowData ==============================================================

test_that("sanitizeRowData", {
    object <- sanitizeRowData(rowRanges(rse))
    expect_s4_class(object, "GRanges")
    df <- as.data.frame(object)
    expect_true(hasRownames(df))
    # FIXME This check is failing on AppVeyor CI.
    expect_identical(
        object = lapply(df, class),
        expected = list(
            seqnames = "factor",
            start = "integer",
            end = "integer",
            width = "integer",
            strand = "factor",
            # FIXME Returning as factor instead of character on BioC 3.7.
            # Returns as character on BioC 3.8.
            geneID = "character",
            geneName = "factor",
            geneBiotype = "factor",
            broadClass = "factor"
        )
    )
})
