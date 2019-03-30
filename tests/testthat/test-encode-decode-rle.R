context("Run-length encoding")

data(df, gr, package = "acidtest")

test_that("encode : DataFrame", {
    x <- encode(df)
    expect_s4_class(x[[1L]], "Rle")
})

test_that("encode : GRanges", {
    x <- encode(gr)
    expect_s4_class(mcols(x)[[1L]], "Rle")
})

test_that("decode : DataFrame", {
    x <- encode(df)
    y <- decode(x)
    expect_is(y[[1L]], "integer")
})

test_that("decode : GRanges", {
    x <- encode(gr)
    y <- decode(x)
    expect_is(mcols(y)[[1L]], "character")
})
