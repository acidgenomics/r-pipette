context("matchRowNameColumn")

test_that("NULL return", {
    expect_null(matchRowNameColumn(data.frame()))
})

test_that("Match failure", {
    expect_error(
        object = matchRowNameColumn(data.frame(rn = "a", rowname = "b")),
        regexp = "Multiple row names columns detected: rn, rowname."
    )
})


test_that("data.table", {
    expect_identical(
        object = matchRowNameColumn(dt),
        expected = "rn"
    )
})

test_that("tbl_df", {
    expect_identical(
        object = matchRowNameColumn(tbl),
        expected = "rowname"
    )
})
