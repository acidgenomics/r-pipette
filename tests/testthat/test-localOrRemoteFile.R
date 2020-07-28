context("localOrRemoteFile")

skip_if_not(hasInternet())

test_that("localOrRemoteFile", {
    urls <- paste(pipetteTestsURL, c("example.csv", "example.rda"), sep = "/")
    files <- localOrRemoteFile(urls)
    expect_is(files, "character")
    expect_match(
        object = basename(files),
        regexp = "pipette-"
    )
})

## `normalizePath()` returns different error messages depending on R version.
## Current: No such file or directory
## AppVeyor: The system cannot find the file specified
test_that("Missing file", {
    expect_error(
        object = localOrRemoteFile("XXX.csv"),
        regexp = "Access"
    )
})

test_that("No file extension", {
    expect_error(
        object = localOrRemoteFile("file"),
        regexp = "Access"
    )
})

test_that(
    "Automatic decompression", {
        for (ext in c("bz2", "gz", "xz", "zip")) {
        file <- file.path("cache", paste0("example.txt.", ext))
        file <- localOrRemoteFile(file)
        expect_identical(readLines(file), "acid")
    }
})
