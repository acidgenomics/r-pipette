test_that("DFrame", {
    x <- DataFrame(
        "aaa" = factor(
            x = c("a", "a", "b", "b"),
            levels = c("a", "b", "c")
        ),
        "bbb" = as.factor(c("b", "b", "c", "c"))
    )
    x <- droplevels2(x)
    expect_s4_class(x, "DFrame")
    expect_true(all(bapply(X = x, FUN = is.factor)))
    expect_identical(
        object = lapply(X = x, FUN = levels),
        expected = list(
            "aaa" = c("a", "b"),
            "bbb" = c("b", "c")
        )
    )
})

test_that("Early return on empty DFrame", {
    x <- DataFrame()
    expect_identical(
        object = droplevels2(x),
        expected = x
    )
})

test_that("GenomicRanges", {
    x <- gr
    mcols(x)[[1L]] <- as.factor(mcols(x)[[1L]])
    expect_true(any(bapply(X = mcols(x), FUN = is.factor)))
    x <- droplevels2(x)
    expect_s4_class(x, "GenomicRanges")
    expect_true(any(bapply(X = mcols(x), FUN = is.factor)))
})

test_that("IRanges", {
    x <- ir
    expect_identical(
        object = droplevels2(x),
        expected = x
    )
})
