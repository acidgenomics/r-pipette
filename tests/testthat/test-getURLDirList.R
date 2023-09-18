## FIXME Add type tests: dirs, files.
## FIXME Add HTTP coverage.
## FIXME Check absolute path support.
## FIXME Consider keeping trailing slash on directories.


test_that("NCBI FTP", {
    url <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
    skip_if_not(isAnExistingURL(url))
    ## FIXME This now isn't working, need to rethink.
    x <- getURLDirList(url = url, pattern = "^refseq/$", absolute = TRUE)
    expect_type(x, "character")
})

test_that("NCBI HTTPS", {
    url <- "https://ftp.ncbi.nlm.nih.gov/genomes/"
    skip_if_not(isAnExistingURL(url))
})
