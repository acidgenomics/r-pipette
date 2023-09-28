dir <- cacheDir

test_that("RDS, overwrite mode", {
    envir <- new.env()
    x <- loadDataAsName(
        new = serialized,
        dir = dir,
        envir = envir,
        overwrite = TRUE
    )
    expect_named(x, "new")
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
    expect_named(x, "data")
})

test_that("Standard evaluation", {
    expect_error(
        object = loadDataAsName(data = "gr.rda", dir = dir),
        regexp = "NSE"
    )
})

test_that("Missing files", {
    expect_error(
        object = loadDataAsName(data = XXX, dir = dir),
        regexp = "missing"
    )
})

test_that("Multiple objects in single file", {
    expect_error(
        object = loadDataAsName(data = multi, dir = dir),
        regexp = "multiple"
    )
})

test_that("Invalid arguments", {
    expect_error(
        object = loadDataAsName(data = gr, dir = "XXX")
    )
    expect_error(
        object = loadDataAsName(data = gr, dir = dir, envir = "XXX"),
        regexp = "is.environment"
    )
})
