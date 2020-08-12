context("loadData")

skip_if_not(hasInternet())

dir <- "cache"

test_that("R data", {
    envir <- new.env()
    x <- loadData(gr, dir = dir, envir = envir)
    expect_identical(x, c(gr = realpath(file.path(dir, "gr.rda"))))
    expect_true(exists("gr", envir = envir, inherits = FALSE))
})

test_that("R data serialized", {
    envir <- new.env()
    x <- loadData(serialized, dir = dir, envir = envir)
    expect_identical(
        object = x,
        expected = c(serialized = realpath(file.path(dir, "serialized.rds")))
    )
})

test_that("Error on mixed extensions (no RDS/RDA soup).", {
    expect_error(
        object = basename(loadData(gr, serialized, dir = dir)),
        regexp = "RDS, RDA, and/or RDATA"
    )
})

test_that("Standard evaluation", {
    expect_error(
        object = loadData("gr.rda", dir = dir),
        regexp = "non-standard evaluation"
    )
})

test_that("List mode", {
    object <- loadData(list = "serialized", dir = "cache")
    expect_identical(basename(object), "serialized.rds")
})

## Avoid accidental reassignment in the current environment.
test_that("Overwrite mode", {
    envir <- new.env()
    envir[["gr"]] <- TRUE
    envir[["serialized"]] <- TRUE

    ## RDS
    expect_error(
        object = loadData(
            serialized,
            dir = dir,
            envir = envir,
            overwrite = FALSE
        ),
        regexp = "overwrite"
    )
    expect_type(
        object = loadData(
            serialized,
            dir = dir,
            envir = envir,
            overwrite = TRUE
        ),
        type = "character"
    )

    ## RDA
    expect_error(
        object = loadData(gr, dir = dir, envir = envir, overwrite = FALSE),
        regexp = "overwrite"
    )
    expect_type(
        object = loadData(gr, dir = dir, envir = envir, overwrite = TRUE),
        type = "character"
    )
})

test_that("Multiple objects in single file", {
    expect_error(
        object = loadData(multi, dir = dir),
        regexp = "'multi.rda' contains multiple objects: x, y"
    )
})

test_that("Renamed file", {
    expect_error(
        object = loadData(renamed, dir = dir),
        regexp = "'renamed.rda' file has been renamed."
    )
})

test_that("Duplicate RDA and RDS files", {
    expect_error(
        object = loadData(example, dir = dir),
        regexp = "'example' is not unique on disk."
    )
})

test_that("Invalid arguments", {
    expect_error(
        object = loadData(gr, dir = "XXX")
    )
    expect_error(
        object = loadData(gr, dir = dir, envir = "XXX"),
        regexp = "is.environment"
    )
})
