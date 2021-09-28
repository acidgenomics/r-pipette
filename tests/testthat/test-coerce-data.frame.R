context("coerce : data.frame")

## Note that `as(object, "data.frame")` should keep `tbl_df` class here.

test_that("S3(ish) and S4", {
    for (object in list(gr, ir, sparse, tbl)) {
        x <- as.data.frame(object)
        expect_is(x, "data.frame")
        x <- as(object, "data.frame")
        expect_is(x, "data.frame")
    }
})

test_that("Ranges", {
    expect_true(hasRownames(as.data.frame(gr)))
    expect_true(hasRownames(as.data.frame(ir)))
    expect_false(hasRownames(as.data.frame(gr, row.names = NULL)))
    expect_false(hasRownames(as.data.frame(ir, row.names = NULL)))
    expect_true(isSubset(colnames(mcols(gr)), colnames(as.data.frame(gr))))
    expect_true(isSubset(colnames(mcols(ir)), colnames(as.data.frame(ir))))
})

test_that("DataFrame with complex S4 columns", {
    ## Note that this method is used internally but not currently exported, so
    ## we don't collide with coercion methods defined by Bioconductor.
    x <- DataFrame(
        "a" = seq(from = 1L, to = 2L),
        "b" = SimpleList("aa" = 1L, "bb" = 2L)
    )
    expect_error(
        object = `.coerce,DFrame,data.frame`(x),
        regexp = "SimpleList"
    )
})
