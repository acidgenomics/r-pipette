test_that("DFrame", {
    expect_identical(
        object = atomize(DataFrame()),
        expected = DataFrame()
    )
    object <- encode(df)
    expect_false(any(bapply(X = object, FUN = is.atomic)))
    object <- atomize(object)
    expect_true(all(bapply(X = object, FUN = is.atomic)))
    expect_s4_class(object, "DFrame")
    expect_true(hasRownames(object))
})

test_that("GenomicRanges", {
    expect_identical(
        object = atomize(GRanges()),
        expected = GRanges()
    )
    object <- encode(gr)
    expect_false(any(bapply(X = mcols(object), FUN = is.atomic)))
    object <- atomize(gr)
    expect_true(all(bapply(X = mcols(object), FUN = is.atomic)))
    expect_s4_class(object, "GenomicRanges")
    expect_true(hasNames(object))
})

test_that("Drop non-atomic column", {
    object <- DataFrame(
        "a" = "a",
        "b" = I(list("a" = seq_len(3L)))
    )
    expect_identical(colnames(object), c("a", "b"))
    object <- atomize(object)
    expect_identical(colnames(object), "a")
})

test_that("Only non-atomic column", {
    object <- DataFrame(
        "a" = I(list("a" = seq_len(3L)))
    )
    expect_identical(colnames(object), "a")
    object <- atomize(object)
    expect_identical(
        object = dim(object),
        expected = c(1L, 0L)
    )
    expect_identical(
        object = dimnames(object),
        expected = list(NULL, character(0L))
    )
})
