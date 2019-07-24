context("loadDataAsName")

skip_if_not(hasInternet())

dir <- "cache"

test_that("RDS, overwrite mode", {
    envir <- new.env()

    x <- loadDataAsName(
        new = serialized,
        dir = dir,
        envir = envir,
        overwrite = TRUE
    )
    expect_identical(names(x), "new")
    ## We're defaulting to global environment.
    expect_true(exists("new", envir = envir, inherits = FALSE))

    expect_error(
        object = loadDataAsName(
            new = serialized,
            dir = dir,
            envir = envir,
            overwrite = FALSE
        ),
        regexp = "overwrite"
    )
})

test_that("RDA", {
    x <- loadDataAsName(data = gr, dir = dir)
    expect_identical(names(x), "data")
})

test_that("Standard evaluation", {
    expect_error(
        object = loadDataAsName(data = "gr.rda", dir = dir),
        regexp = "non-standard evaluation"
    )
})

test_that("Missing files", {
    expect_error(
        object = loadDataAsName(data = XXX, dir = dir),
        regexp = rdataLoadError
    )
})

test_that("Multiple objects in single file", {
    expect_error(
        object = loadDataAsName(data = multi, dir = dir),
        regexp = "multi.rda contains multiple objects: x, y"
    )
})

test_that("Invalid arguments", {
    expect_error(
        object = loadDataAsName(data = gr, dir = "XXX"),
        regexp = "XXX"
    )
    expect_error(
        object = loadDataAsName(data = gr, dir = dir, envir = "XXX"),
        regexp = "is.environment"
    )
})
