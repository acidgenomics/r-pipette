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
    skip_if_not_installed("data.table")
    data(data.table, package = "AcidTest", envir = environment())
    object <- data.table
    expect_identical(
        object = matchRownameColumn(object),
        expected = "rn"
    )
})

test_that("tbl_df", {
    skip_if_not_installed("tibble")
    data(tibble, package = "AcidTest", envir = environment())
    object <- tibble
    expect_identical(
        object = matchRownameColumn(object),
        expected = "rowname"
    )
})
