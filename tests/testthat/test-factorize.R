context("factorize")

test_that("DFrame", {
    object <- factorize(df)
    expect_s4_class(object, "DFrame")
    expect_true(
        all(vapply(X = object, FUN = is.factor, FUN.VALUE = logical(1L)))
    )
})
