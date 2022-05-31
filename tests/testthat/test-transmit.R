## Note that only FTP is currently supported.
remoteDir <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
skip_if_not(hasInternet(remoteDir))

test_that("Get README file", {
    expected <- file.path(tempdir, "README.txt")
    unlink(expected)
    object <- transmit(
        remoteDir = remoteDir,
        localDir = tempdir,
        pattern = "^README\\.txt$",
        compress = FALSE
    )
    names(expected) <- "README.txt"
    expect_identical(object, expected)
    ## Check that function skips on existing files.
    expect_message(
        object = transmit(
            remoteDir = remoteDir,
            localDir = tempdir,
            pattern = "^README\\.txt$",
            compress = FALSE
        ),
        regexp = "Skipped"
    )
    unlink(object)
})

test_that("Rename and compress", {
    expected <- file.path(tempdir, "readme.txt.gz")
    unlink(expected)
    object <- transmit(
        remoteDir = remoteDir,
        localDir = tempdir,
        pattern = "^README\\.txt$",
        rename = "readme.txt",
        compress = TRUE
    )
    names(expected) <- "README.txt"
    expect_identical(object, expected)
    unlink(object)
})

test_that("URL return", {
    url <- transmit(
        remoteDir = pasteURL(
            "ftp.pantherdb.org",
            "sequence_classifications",
            "current_release",
            "PANTHER_Sequence_Classification_files",
            protocol = "ftp"
        ),
        pattern = "human",
        download = FALSE
    )
    expect_true(isAURL(url))
})

test_that("Invalid parameters", {
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
            pattern = "^README$",
            rename = c("XXX", "YYY")
        ),
        regexp = "match"
    )
    ## Currently only FTP is supported.
    expect_error(
        object = transmit(
            remoteDir = "http://steinbaugh.com",
            pattern = "^README$"
        ),
        regexp = "ftp"
    )
    expect_error(
        object = transmit(
            remoteDir = "ftp://ftp.wormbase.org/pub/",
            pattern = "^README$"
        ),
        regexp = "remoteFiles"
    )
})
