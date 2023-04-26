test_that("DFrame", {
    x <- df
    x <- encode(x)
    expect_s4_class(x[[1L]], "Rle")
    y <- decode(x)
    expect_s3_class(y[[1L]], "factor")
})

test_that("GRanges", {
    x <- encode(gr)
    expect_s4_class(mcols(x)[[1L]], "Rle")
    y <- decode(x)
    expect_type(mcols(y)[[1L]], "character")
})
