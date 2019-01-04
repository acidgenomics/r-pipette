# loadData =====================================================================
test_that("loadData", {
    # R data.
    expect_identical(
        object = loadData(gr),
        expected = c(gr = realpath("gr.rda"))
    )
    # We're defaulting to global environment.
    expect_true(exists("gr", envir = globalenv(), inherits = FALSE))

    # R data serialized.
    expect_identical(
        object = loadData(serialized),
        expected = c(
            serialized = realpath("serialized.rds")
        )
    )
})

# Don't allow RDS/RDA soup.
test_that("loadData : Mixed extensions", {
    expect_error(
        object = loadData(gr, serialized) %>% basename(),
        regexp = "RDS/RDA/RDATA"
    )
})

test_that("loadData : Standard evaluation", {
    expect_error(
        object = loadData("gr.rda"),
        regexp = "non-standard evaluation"
    )
})

# Avoid accidental reassignment in the current environment.
test_that("loadData : Already exists", {
    gr <- TRUE
    expect_error(
        object = loadData(gr),
        regexp = "reassignment"
    )
})

test_that("loadData : Multiple objects in single file", {
    expect_error(
        object = loadData(multi),
        regexp = "multi.rda contains multiple objects: x, y"
    )
})

test_that("loadData : Renamed file", {
    expect_error(
        object = loadData(renamed),
        regexp = "renamed.rda has been renamed."
    )
})

test_that("loadData : Duplicate RDA and RDS files", {
    expect_error(
        object = loadData(example),
        regexp = "example is not unique on disk."
    )
})

test_that("loadData : Invalid arguments", {
    expect_error(
        object = loadData(gr, dir = "XXX"),
        regexp = "hasAccess"
    )
    expect_error(
        object = loadData(gr, envir = "XXX"),
        regexp = "is.environment"
    )
})



# loadDataAsName ===============================================================
test_that("loadDataAsName : Non-standard evaluation", {
    object <- loadDataAsName(data_1 = gr, data_2 = mn)
    expect_is(object, "character")
    expect_identical(names(object), c("data_1", "data_2"))
    # We're defaulting to global environment.
    expect_true(exists("data_1", envir = globalenv(), inherits = FALSE))
    expect_true(exists("data_2", envir = globalenv(), inherits = FALSE))
    # Now that the objects are loaded, let's check to make sure we can't
    # accidentally overwrite in the current environment.
    expect_error(
        object = loadDataAsName(data_1 = gr, data_2 = mn),
        regexp = "reassignment"
    )
})


test_that("loadDataAsName : Serialized", {
    object <- loadDataAsName(new = serialized)
    expect_identical(names(object), "new")
    # We're defaulting to global environment.
    expect_true(exists("new", envir = globalenv(), inherits = FALSE))
})

test_that("loadData : Standard evaluation", {
    expect_error(
        object = loadDataAsName(data = "gr.rda"),
        regexp = "non-standard evaluation"
    )
})

test_that("loadDataAsName : Missing files", {
    expect_error(
        object = loadDataAsName(data = XXX),
        regexp = rdataLoadError
    )
})

test_that("loadDataAsName : Multiple objects in single file", {
    expect_error(
        object = loadDataAsName(data = multi),
        regexp = "multi.rda contains multiple objects: x, y"
    )
})

test_that("loadDataAsName : Invalid arguments", {
    expect_error(
        object = loadDataAsName(data = gr, dir = "XXX"),
        regexp = "hasAccess"
    )
    expect_error(
        object = loadDataAsName(data = gr, envir = "XXX"),
        regexp = "is.environment"
    )
})



# loadRemoteData ===============================================================
test_that("loadRemoteData", {
    url <- paste(url, "example.rds", sep = "/")
    object <- loadRemoteData(url)
    # Character matrix of loaded files.
    expect_is(object, "character")
    expect_identical(object, c(example = url))
    # Check that the object loaded correctly.
    expect_is(example, "DataFrame")
})

test_that("loadRemoteData : Already loaded", {
    example <- TRUE
    expect_error(
        object = loadRemoteData(url = paste(url, "example.rda", sep = "/"))
    )
})

test_that("loadRemoteData : Invalid arguments", {
    expect_error(
        object = loadRemoteData(url = paste(url, "mmusculus.gtf", sep = "/")),
        regexp = rdataLoadError
    )
    expect_error(
        object = loadRemoteData("foobar.rda"),
        regexp = "URL"
    )
    expect_error(
        object = loadRemoteData(
            url = paste(paste(url, "example.rda", sep = "/")),
            envir = "XXX"
        ),
        regexp = "is.environment"
    )
})
