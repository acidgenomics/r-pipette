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
