context("coerce : SummarizedExperiment")

## Easy way to test an S4 class that extends SE without importing?
test_that("S3 coercion that preserves row data", {
    for (object in list(
        rse = rse,
        se = as.SummarizedExperiment(rse)
    )) {
        object <- as.SummarizedExperiment(object)
        expect_s4_class(object, "SummarizedExperiment")
        expect_named(rowData(object))
        expect_named(metadata(rowData(object)))
    }
})
