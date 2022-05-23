test_that("sanitizePercent", {
    object <- c("100%", "10.0%", "1%", "0.1%", "0.01%")
    expect_identical(
        sanitizePercent(object),
        c(1e+00, 1e-01, 1e-02, 1e-03, 1e-04) # nolint
    )
})

test_that("Don't modify character if no match", {
    object <- c("a", "b")
    expect_identical(sanitizePercent(object), object)
})

test_that("Don't modify atomic", {
    expect_identical(sanitizePercent(NA), NA)
})
