context("File downloads")

test_that("localOrRemoteFile : Vectorized", {
    urls <- paste(brioCacheURL, c("example.csv", "example.rda"), sep = "/")
    files <- localOrRemoteFile(urls)
    expect_is(files, "character")
    expect_identical(basename(urls), basename(files))
})

# `normalizePath() returns different error messages depending on the R version.
# Current: No such file or directory
# AppVeyor: The system cannot find the file specified
test_that("localOrRemoteFile : Missing file", {
    expect_error(
        object = localOrRemoteFile("XXX.csv"),
        regexp = "path\\[1\\]"
    )
})



context("Transmit (FTP file matching)")

# Note that only FTP is currently supported.
remoteDir <- paste(
    "ftp://ftp.pantherdb.org",
    "sequence_classifications",
    "current_release",
    sep = "/"
)

test_that("transmit", {
    skip_on_travis()

    object <- transmit(
        remoteDir = remoteDir,
        pattern = "README",
        compress = FALSE
    )
    expected <- file.path(getwd(), "README")
    names(expected) <- "README"
    expect_identical(object, expected)

    # Check that function skips on existing.
    expect_message(
        object = transmit(
            remoteDir = remoteDir,
            pattern = "README",
            compress = FALSE
        ),
        regexp = "All files are already downloaded."
    )

    unlink("README")
})

test_that("transmit : Rename and compress", {
    skip_on_travis()

    object <- transmit(
        remoteDir = remoteDir,
        pattern = "README",
        rename = "readme.txt",
        compress = TRUE
    )
    expected <- file.path(getwd(), "readme.txt.gz")
    names(expected) <- "README"
    expect_identical(object, expected)

    unlink("readme.txt.gz")
})

test_that("transmit : Invalid parameters", {
    skip_on_travis()
    expect_error(
        object = transmit(
            remoteDir = "http://steinbaugh.com",
            pattern = "README"
        ),
        regexp = "ftp"
    )
    expect_error(
        object = transmit(
            remoteDir = "ftp://ftp.wormbase.org/pub/",
            pattern = "README"
        ),
        regexp = "remoteFiles"
    )
    expect_error(
        object = transmit(
            remoteDir = remoteDir,
            pattern = "XXX"
        ),
        regexp = "match"
    )
    expect_error(
        object = transmit(
            remoteDir = remoteDir,
            pattern = "README",
            rename = c("XXX", "YYY")
        ),
        regexp = "areSameLength"
    )
})
