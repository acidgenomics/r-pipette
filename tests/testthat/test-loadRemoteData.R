context("loadRemoteData")

test_that("loadRemoteData", {
    envir <- new.env()
    url <- paste(brioTestsURL, "example.rds", sep = "/")
    object <- loadRemoteData(url, envir = envir)
    # Character matrix of loaded files.
    expect_is(object, "character")
    expect_identical(object, c(example = url))
    # Check that the object loaded correctly.
    expect_is(envir[["example"]], "DataFrame")
})

test_that("Overwrite mode", {
    envir <- new.env()
    envir[["example"]] <- TRUE
    expect_error(
        object = loadRemoteData(
            url = paste(brioTestsURL, "example.rda", sep = "/"),
            envir = envir,
            overwrite = FALSE
        ),
        regexp = "overwrite"
    )
    expect_type(
        loadRemoteData(
            url = paste(brioTestsURL, "example.rda", sep = "/"),
            envir = envir,
            overwrite = TRUE
        ),
        type = "character"
    )
})

test_that("Invalid arguments", {
    expect_error(
        loadRemoteData(paste(brioTestsURL, "mmusculus.gtf", sep = "/")),
        rdataLoadError
    )
    expect_error(
        object = loadRemoteData("foobar.rda"),
        regexp = "URL"
    )
    expect_error(
        object = loadRemoteData(
            url = paste(paste(brioTestsURL, "example.rda", sep = "/")),
            envir = "XXX"
        ),
        regexp = "is.environment"
    )
})
