context("as.DataFrame")

test_that("list method", {
    from <- list(
        "a" = list(c(1L, 2L), c(3L, 4L)),
        "b" = list(NULL, NULL)
    )
    to <- as.DataFrame(from)
    expect_s4_class(to, "DataFrame")
    expect_identical(dim(to), c(2L, 2L))
    expect_identical(
        object = vapply(X = to, FUN = class, FUN.VALUE = character(1L)),
        expected = c("a" = "list", "b" = "list")
    )
})
