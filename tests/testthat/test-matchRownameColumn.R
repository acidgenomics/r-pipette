context("matchRownameColumn")

test_that("NULL return", {
    df <- data.frame()
    expect_null(matchRownameColumn(df))
})

test_that("Match failure", {
    df <- data.frame(rn = "a", rowname = "b")
    expect_error(
        object = matchRownameColumn(df),
        regexp = "Multiple"
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
