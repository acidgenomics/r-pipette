context("encode/decode")

test_that("DataFrame", {
    x <- encode(df)
    expect_s4_class(x[[1L]], "Rle")
    y <- decode(x)
    expect_is(y[[1L]], "integer")
})

test_that("GRanges", {
    x <- encode(gr)
    expect_s4_class(mcols(x)[[1L]], "Rle")
    y <- decode(x)
    expect_type(mcols(y)[[1L]], "character")
})

test_that("SummarizedExperiment", {
    x <- encode(rse)
    expect_s4_class(rowData(x)[[1L]], "Rle")
    expect_s4_class(colData(x)[[1L]], "Rle")
    y <- decode(rse)
    expect_s3_class(rowData(y)[[1L]], "factor")
    expect_s3_class(colData(y)[[1L]], "factor")
})
