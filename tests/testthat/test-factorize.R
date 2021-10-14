context("factorize")

test_that("DataFrame", {
    object <- factorize(df)
    expect_s4_class(object, "DataFrame")
    expect_true(
        all(vapply(X = object, FUN = is.factor, FUN.VALUE = logical(1L)))
    )
})
