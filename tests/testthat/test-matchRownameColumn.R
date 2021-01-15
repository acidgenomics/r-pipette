context("matchRownameColumn")

test_that("NULL return", {
    expect_null(matchRownameColumn(data.frame()))
})

test_that("Match failure", {
    expect_error(
        object = matchRownameColumn(data.frame(rn = "a", rowname = "b")),
        regexp = "Multiple row names columns detected: rn, rowname."
    )
})


test_that("data.table", {
    expect_identical(
        object = matchRownameColumn(dt),
        expected = "rn"
    )
})

test_that("tbl_df", {
    expect_identical(
        object = matchRownameColumn(tbl),
        expected = "rowname"
    )
})
