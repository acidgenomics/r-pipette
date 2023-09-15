test_that("NCBI FTP", {
    url <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
    skip_if_not(isAnExistingURL(url))
    x <- getURLDirList(url, pattern = "^refseq$")
    expect_type(x, "character")
})

test_that("FTP only", {
    url <- "https://ftp.ensembl.org/pub/"
    expect_error(
        object = getURLDirList(url),
        regexp = "ftp"
    )
})
