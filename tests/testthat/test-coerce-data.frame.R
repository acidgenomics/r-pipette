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
