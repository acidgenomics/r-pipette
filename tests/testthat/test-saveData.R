context("saveData")

dir <- "example"
paths <- file.path(getwd(), "example", c("rse.rda", "sce.rda"))
names(paths) <- c("rse", "sce")

test_that("R data", {
    object <- saveData(
        rse, sce,
        ext = "rda",
        dir = dir,
        overwrite = TRUE
    )
    expect_identical(object, paths)
})

test_that("R data serialized", {
    object <- saveData(
        rse, sce,
        ext = "rds",
        dir = dir,
        overwrite = TRUE
    )
    expect_identical(
        object = basename(object),
        expected = c("rse.rds", "sce.rds")
    )
})

test_that("overwrite = FALSE", {
    expect_warning(
        object = saveData(
            rse, sce,
            dir = dir, overwrite = FALSE
        ),
        regexp = "No files were saved."
    )
    unlink(dir, recursive = TRUE)
})

test_that("Invalid parameters", {
    expect_error(
        object = saveData(XXX),
        regexp = "object 'XXX' not found"
    )
    expect_error(
        object = saveData("example"),
        regexp = "non-standard evaluation"
    )
    expect_error(
        object = saveData(rse, dir = NULL),
        regexp = "isString"
    )
})
