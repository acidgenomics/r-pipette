context("writeCounts")

test_that("writeCounts", {
    dir <- "example"
    expect_message(
        object = writeCounts(mat, sparse, dir = dir, compress = TRUE),
        regexp = "Writing mat, sparse"
    )
    expect_identical(
        object = list.files(dir),
        expected = c(
            "mat.csv.gz",
            "sparse.mtx.gz",
            "sparse.mtx.gz.colnames",
            "sparse.mtx.gz.rownames"
        )
    )
    unlink(dir, recursive = TRUE)
})

test_that("Require a matrix, and don't allow data frames.", {
    expect_error(
        object = writeCounts(mtcars),
        regexp = "mtcars is not a matrix"
    )
})

test_that("Check that `eval_bare` call errors on missing object.", {
    expect_error(
        object = writeCounts(XXX),
        regexp = "object 'XXX' not found"
    )
})
