context("atomize")

test_that("DataFrame", {
    data <- rowData(rse)
    ## Fix for gene identifier rename in AcidTest.
    colnames(data) <- camelCase(colnames(data), strict = TRUE)
    rownames(data) <- rownames(rse)
    object <- atomize(data)
    expect_s4_class(object, "DataFrame")
    expect_true(hasRownames(object))
    expect_identical(
        object = lapply(object, class),
        expected = list(
            "broadClass" = "factor",
            "description" = "character",
            "geneBiotype" = "character",
            "geneId" = "character",
            "geneIdVersion" = "character",
            "geneName" = "character",
            "seqCoordSystem" = "character"
        )
    )
})

test_that("GRanges", {
    gr <- rowRanges(rse)
    names(mcols(gr)) <-
        camelCase(names(mcols(gr)), strict = TRUE)
    object <- atomize(gr)
    expect_s4_class(object, "GRanges")
    expect_true(hasNames(object))
    expect_identical(
        object = lapply(mcols(object), class),
        expected = list(
            "broadClass" = "factor",
            "description" = "character",
            "geneBiotype" = "character",
            "geneId" = "character",
            "geneIdVersion" = "character",
            "geneName" = "character",
            "seqCoordSystem" = "character"
        )
    )
})
