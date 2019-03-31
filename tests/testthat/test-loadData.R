context("loadData")

test_that("loadData", {
    envir <- new.env()

    # R data.
    expect_identical(
        object = loadData(gr, dir = ".", envir = envir),
        expected = c(gr = realpath("gr.rda"))
    )
    # Note that we're defaulting to global environment.
    expect_true(exists("gr", envir = envir, inherits = FALSE))

    # R data serialized.
    expect_identical(
        object = loadData(serialized, dir = ".", envir = envir),
        expected = c(
            serialized = realpath("serialized.rds")
        )
    )
})

# Don't allow RDS/RDA soup.
test_that("Mixed extensions", {
    expect_error(
        object = basename(loadData(gr, serialized, dir = ".")),
        regexp = "RDS/RDA/RDATA"
    )
})

test_that("Standard evaluation", {
    expect_error(
        object = loadData("gr.rda", dir = "."),
        regexp = "non-standard evaluation"
    )
})

# Avoid accidental reassignment in the current environment.
test_that("Object already exists", {
    envir <- new.env()
    envir[["gr"]] <- TRUE
    expect_error(
        object = loadData(gr, dir = ".", envir = envir),
        regexp = "reassignment"
    )
})

test_that("Multiple objects in single file", {
    expect_error(
        object = loadData(multi, dir = "."),
        regexp = "multi.rda contains multiple objects: x, y"
    )
})

test_that("Renamed file", {
    expect_error(
        object = loadData(renamed, dir = "."),
        regexp = "renamed.rda has been renamed."
    )
})

test_that("Duplicate RDA and RDS files", {
    expect_error(
        object = loadData(example, dir = "."),
        regexp = "example is not unique on disk."
    )
})

test_that("Invalid arguments", {
    expect_error(
        object = loadData(gr, dir = "XXX"),
        regexp = "path\\[1\\]"
    )
    expect_error(
        object = loadData(gr, dir = ".", envir = "XXX"),
        regexp = "is.environment"
    )
})
