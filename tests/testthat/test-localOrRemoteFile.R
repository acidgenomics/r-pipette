context("localOrRemoteFile")

test_that("localOrRemoteFile", {
    urls <- paste(brioTestsURL, c("example.csv", "example.rda"), sep = "/")
    files <- localOrRemoteFile(urls)
    expect_is(files, "character")
    expect_identical(basename(urls), basename(files))
})

# `normalizePath() returns different error messages depending on the R version.
# Current: No such file or directory
# AppVeyor: The system cannot find the file specified
test_that("Missing file", {
    expect_error(
        object = localOrRemoteFile("XXX.csv"),
        regexp = "path\\[1\\]"
    )
})
