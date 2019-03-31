context("encode")

test_that("DataFrame", {
    x <- encode(df)
    expect_s4_class(x[[1L]], "Rle")
})

test_that("GRanges", {
    x <- encode(gr)
    expect_s4_class(mcols(x)[[1L]], "Rle")
})



context("decode")

test_that("DataFrame", {
    x <- encode(df)
    y <- decode(x)
    expect_is(y[[1L]], "integer")
})

test_that("GRanges", {
    x <- encode(gr)
    y <- decode(x)
    expect_is(mcols(y)[[1L]], "character")
})
