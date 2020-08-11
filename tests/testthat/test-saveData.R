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
    expect_message(
        object = saveData(
            rse, sce,
            dir = dir, overwrite = FALSE
        ),
        regexp = "No files were saved."
    )
    unlink(dir, recursive = TRUE)
})

test_that("List mode", {
    x <- TRUE
    y <- FALSE
    object <- saveData(list = c("x", "y"), dir = "XXX")
    expect_identical(
        object = basename(object),
        expected = c("x.rds", "y.rds")
    )
    expect_true(all(file.exists(file.path("XXX", paste0(c("x", "y"), ".rds")))))
    unlink("XXX", recursive = TRUE)
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
