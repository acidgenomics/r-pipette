context("atomize")

test_that("DataFrame", {
    object <- encode(df)
    expect_false(any(bapply(X = object, FUN = is.atomic)))
    object <- atomize(object)
    expect_true(all(bapply(X = object, FUN = is.atomic)))
    expect_s4_class(object, "DataFrame")
    expect_true(hasRownames(object))
})

test_that("GRanges", {
    object <- encode(gr)
    expect_false(any(bapply(X = mcols(object), FUN = is.atomic)))
    object <- atomize(gr)
    expect_true(all(bapply(X = mcols(object), FUN = is.atomic)))
    expect_s4_class(object, "GRanges")
    expect_true(hasNames(object))
})
