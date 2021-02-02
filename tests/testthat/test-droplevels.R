context("droplevels")

Rle <- structure("Rle", package = "S4Vectors")  # nolint

test_that("DataFrame", {
    x <- DataFrame(
        "aaa" = factor(
            x = c("a", "a", "b", "b"),
            levels = c("a", "b", "c")
        ),
        "bbb" = as.factor(c("b", "b", "c", "c"))
    )
    x <- droplevels(x)
    expect_s4_class(x, "DataFrame")
    expect_true(all(bapply(X = x, FUN = is.factor)))
    expect_identical(
        object = lapply(X = x, FUN = levels),
        expected = list(
            "aaa" = c("a", "b"),
            "bbb" = c("b", "c")
        )
    )
})

test_that("Early return on empty DataFrame", {
    expect_identical(droplevels(DataFrame()), DataFrame())
})

test_that("IRanges", {
    expect_identical(droplevels(ir), ir)
})

test_that("GRanges", {
    x <- GRanges
    mcols(x)[[1L]] <- as.factor(mcols(x)[[1L]])
    expect_true(any(bapply(X = mcols(x), FUN = is.factor)))
    x <- droplevels(x)
    expect_s4_class(x, "GRanges")
    expect_true(any(bapply(X = mcols(x), FUN = is.factor)))
})
