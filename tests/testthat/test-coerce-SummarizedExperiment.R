context("coerce : SummarizedExperiment")

## Easy way to test an S4 class that extends SE without importing?
with_parameters_test_that(
    "S3 coercion that preserves row data", {
        object <- as.SummarizedExperiment(object)
        expect_s4_class(object, "SummarizedExperiment")
        expect_named(rowData(object))
        expect_named(metadata(rowData(object)))
    },
    object = list(
        rse = rse,
        se = as.SummarizedExperiment(rse)
    )
)
