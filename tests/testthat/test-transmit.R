## Note that only FTP is currently supported.
remoteDir <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
skip_if_not(isAnExistingUrl(remoteDir))

test_that("Get README file", {
    tempdir <- tempdir2()
    expected <- file.path(tempdir, "README.txt")
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
    unlink2(object)
})

test_that("Rename and compress", {
    tempdir <- tempdir2()
    expected <- file.path(tempdir, "readme.txt.gz")
    object <- transmit(
        remoteDir = remoteDir,
        localDir = tempdir,
        pattern = "^README\\.txt$",
        rename = "readme.txt",
        compress = TRUE
    )
    names(expected) <- "README.txt"
    expect_identical(object, expected)
    unlink2(object)
})

test_that("URL return", {
    skip_if_not(isAnExistingUrl("ftp://ftp.pantherdb.org/"))
    url <- transmit(
        remoteDir = pasteUrl(
            "ftp.pantherdb.org",
            "sequence_classifications",
            "current_release",
            "PANTHER_Sequence_Classification_files",
            protocol = "ftp"
        ),
        pattern = "human",
        download = FALSE
    )
    expect_true(isAUrl(url))
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
})

## > test_that("Invalid parameters : ftp.wormbase.org", {
## >     remoteDir <- "ftp://ftp.wormbase.org/pub/"
## >     skip_if_not(isAnExistingUrl(remoteDir))
## >     expect_error(
## >         object = transmit(
## >             remoteDir = remoteDir,
## >             pattern = "^README$"
## >         ),
## >         regexp = "remoteFiles"
## >     )
## > })
