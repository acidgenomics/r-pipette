## FIXME Add type tests: dirs, files.
## FIXME Add HTTP coverage.
## FIXME Check absolute path support.

test_that("NCBI FTP", {
    url <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
    skip_if_not(isAnExistingURL(url))
    ## FIXME This test isn't correct, need to rework.
    x <- getURLDirList(url = url, pattern = "^refseq/$", absolute = TRUE)

    x <- getURLDirList(url = url, pattern = "^refseq$", absolute = TRUE)

    ## FIXME Add a test without trailing slash, which should fail.
    expect_type(x, "character")
})

test_that("NCBI HTTPS", {
    url <- "https://ftp.ncbi.nlm.nih.gov/genomes/"
    skip_if_not(isAnExistingURL(url))
})
