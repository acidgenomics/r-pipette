skip_if_not(hasInternet())

test_that("loadRemoteData", {
    envir <- new.env()
    url <- pasteUrl(pipetteTestsUrl, "example.rds")
    object <- loadRemoteData(url, envir = envir)
    ## Character matrix of loaded files.
    expect_type(object, "character")
    expect_identical(object, c(example = url))
    ## Check that the object loaded correctly.
    expect_s4_class(envir[["example"]], "DFrame")
})

test_that("Overwrite mode", {
    envir <- new.env()
    envir[["example"]] <- TRUE
    expect_error(
        object = loadRemoteData(
            url = pasteUrl(pipetteTestsUrl, "example.rda"),
            envir = envir,
            overwrite = FALSE
        ),
        regexp = "overwrite"
    )
    expect_type(
        loadRemoteData(
            url = pasteUrl(pipetteTestsUrl, "example.rda"),
            envir = envir,
            overwrite = TRUE
        ),
        type = "character"
    )
})

test_that("Invalid arguments", {
    expect_error(
        object = loadRemoteData(
            url = pasteUrl(pipetteTestsUrl, "example.csv")
        ),
        regexp = "rds"
    )
    expect_error(
        object = loadRemoteData("foobar.rda"),
        regexp = "URL"
    )
    expect_error(
        object = loadRemoteData(
            url = pasteUrl(pipetteTestsUrl, "example.rda"),
            envir = "XXX"
        ),
        regexp = "is.environment"
    )
})
