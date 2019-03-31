context("factorize")

test_that("factorize", {
    df <- DataFrame(
        a = letters[seq_len(5L)],
        b = seq_len(5L)
    )
    x <- factorize(df)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        vapply(x, class, character(1L)),
        c(a = "factor", b = "factor")
    )
})
