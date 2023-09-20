test_that("DFrame", {
    x <- encode(df)
    expect_s4_class(x[[1L]], "Rle")
    y <- decode(x)
    expect_s3_class(y[[1L]], "factor")
    ## Support specific column selection.
    y <- encode(df, j = 1L)
    expect_s4_class(y[[1L]], "Rle")
    expect_s3_class(y[[2L]], "factor")
    y <- encode(df, j = colnames(df)[[1L]])
    expect_s4_class(y[[1L]], "Rle")
    expect_s3_class(y[[2L]], "factor")
    y <- decode(x, j = 1L)
    expect_s3_class(y[[1L]], "factor")
    expect_s4_class(y[[2L]], "Rle")
    y <- decode(x, j = colnames(x)[[1L]])
    expect_s3_class(y[[1L]], "factor")
    expect_s4_class(y[[2L]], "Rle")
    ## Error on invalid column selection.
    expect_error(
        object = encode(df, j = seq(from = 1L, to = ncol(df) + 1L)),
        regexp = "length"
    )
    expect_error(
        object = encode(df, j = "xxx"),
        regexp = "isSubset"
    )
    expect_error(
        object = decode(x, j = seq(from = 1L, to = ncol(x) + 1L)),
        regexp = "length"
    )
    expect_error(
        object = decode(x, j = "xxx"),
        regexp = "isSubset"
    )
})

test_that("GRanges", {
    x <- encode(gr)
    expect_s4_class(mcols(x)[[1L]], "Rle")
    y <- decode(x)
    expect_type(mcols(y)[[1L]], "character")
})
