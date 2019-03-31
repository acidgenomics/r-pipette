context("loadDataAsName")

test_that("Non-standard evaluation", {
    envir <- new.env()
    object <- loadDataAsName(
        new = serialized,
        dir = ".",
        envir = envir
    )
    expect_identical(names(object), "new")
    # We're defaulting to global environment.
    expect_true(exists("new", envir = envir, inherits = FALSE))
    # Now that the objects are loaded, let's check to make sure we can't
    # accidentally overwrite in the current environment.
    expect_error(
        object = loadDataAsName(
            new = serialized,
            dir = ".",
            envir = envir
        ),
        regexp = "reassignment"
    )
})

test_that("Standard evaluation", {
    expect_error(
        object = loadDataAsName(data = "gr.rda", dir = "."),
        regexp = "non-standard evaluation"
    )
})

test_that("Missing files", {
    expect_error(
        object = loadDataAsName(data = XXX, dir = "."),
        regexp = rdataLoadError
    )
})

test_that("Multiple objects in single file", {
    expect_error(
        object = loadDataAsName(data = multi, dir = "."),
        regexp = "multi.rda contains multiple objects: x, y"
    )
})

test_that("Invalid arguments", {
    expect_error(
        object = loadDataAsName(data = gr, dir = "XXX"),
        regexp = "path\\[1\\]"
    )
    expect_error(
        object = loadDataAsName(data = gr, dir = ".", envir = "XXX"),
        regexp = "is.environment"
    )
})
