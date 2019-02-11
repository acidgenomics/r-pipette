context("Load")



# loadData =====================================================================
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
test_that("loadData : Mixed extensions", {
    expect_error(
        object = basename(loadData(gr, serialized, dir = ".")),
        regexp = "RDS/RDA/RDATA"
    )
})

test_that("loadData : Standard evaluation", {
    expect_error(
        object = loadData("gr.rda", dir = "."),
        regexp = "non-standard evaluation"
    )
})

# Avoid accidental reassignment in the current environment.
test_that("loadData : Already exists", {
    envir <- new.env()
    envir[["gr"]] <- TRUE
    expect_error(
        object = loadData(gr, dir = ".", envir = envir),
        regexp = "reassignment"
    )
})

test_that("loadData : Multiple objects in single file", {
    expect_error(
        object = loadData(multi, dir = "."),
        regexp = "multi.rda contains multiple objects: x, y"
    )
})

test_that("loadData : Renamed file", {
    expect_error(
        object = loadData(renamed, dir = "."),
        regexp = "renamed.rda has been renamed."
    )
})

test_that("loadData : Duplicate RDA and RDS files", {
    expect_error(
        object = loadData(example, dir = "."),
        regexp = "example is not unique on disk."
    )
})

test_that("loadData : Invalid arguments", {
    expect_error(
        object = loadData(gr, dir = "XXX"),
        regexp = "path\\[1\\]"
    )
    expect_error(
        object = loadData(gr, dir = ".", envir = "XXX"),
        regexp = "is.environment"
    )
})



# loadDataAsName ===============================================================
test_that("loadDataAsName : Non-standard evaluation", {
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

test_that("loadData : Standard evaluation", {
    expect_error(
        object = loadDataAsName(data = "gr.rda", dir = "."),
        regexp = "non-standard evaluation"
    )
})

test_that("loadDataAsName : Missing files", {
    expect_error(
        object = loadDataAsName(data = XXX, dir = "."),
        regexp = rdataLoadError
    )
})

test_that("loadDataAsName : Multiple objects in single file", {
    expect_error(
        object = loadDataAsName(data = multi, dir = "."),
        regexp = "multi.rda contains multiple objects: x, y"
    )
})

test_that("loadDataAsName : Invalid arguments", {
    expect_error(
        object = loadDataAsName(data = gr, dir = "XXX"),
        regexp = "path\\[1\\]"
    )
    expect_error(
        object = loadDataAsName(data = gr, dir = ".", envir = "XXX"),
        regexp = "is.environment"
    )
})



# loadRemoteData ===============================================================
test_that("loadRemoteData", {
    envir <- new.env()
    url <- paste(brioCacheURL, "example.rds", sep = "/")
    object <- loadRemoteData(url, envir = envir)
    # Character matrix of loaded files.
    expect_is(object, "character")
    expect_identical(object, c(example = url))
    # Check that the object loaded correctly.
    expect_is(envir[["example"]], "DataFrame")
})

test_that("loadRemoteData : Already loaded", {
    envir <- new.env()
    envir[["example"]] <- TRUE
    expect_error(
        object = loadRemoteData(
            url = paste(brioCacheURL, "example.rda", sep = "/"),
            envir = envir
        ),
        regexp = "reassignment"
    )
})

test_that("loadRemoteData : Invalid arguments", {
    expect_error(
        loadRemoteData(paste(brioCacheURL, "mmusculus.gtf", sep = "/")),
        rdataLoadError
    )
    expect_error(
        object = loadRemoteData("foobar.rda"),
        regexp = "URL"
    )
    expect_error(
        object = loadRemoteData(
            url = paste(paste(brioCacheURL, "example.rda", sep = "/")),
            envir = "XXX"
        ),
        regexp = "is.environment"
    )
})
