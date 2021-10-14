context("coerce : DataFrame")

test_that("S4 'as()' coercion", {
    for (object in list(dt, sparse, tbl)) {
        x <- as(object, "DataFrame")
        expect_s4_class(x, "DataFrame")
        expect_true(hasRownames(x))
    }
})

test_that("as.DFrame list method", {
    from <- list(
        a = list(c(1L, 2L), c(3L, 4L)),
        b = list(NULL, NULL)
    )
    to <- as.DFrame(from)
    expect_s4_class(to, "DFrame")
    expect_identical(dim(to), c(2L, 2L))
    expect_identical(
        object = vapply(X = to, FUN = class, FUN.VALUE = character(1L)),
        expected = c(a = "list", b = "list")
    )
})
