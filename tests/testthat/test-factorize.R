context("factorize")

test_that("factorize", {
    df <- DataFrame(
        a = letters[seq_len(5)],
        b = seq_len(5)
    )
    x <- factorize(df)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        vapply(x, class, character(1L)),
        c(a = "factor", b = "factor")
    )
})
