context("getURLDirList")

skip_if_not(hasInternet())
skip_on_appveyor()
skip_on_docker()

test_that("Ensembl FTP", {
    url <- "ftp://ftp.ensembl.org/pub/"
    x <- getURLDirList(url, pattern = "^release-")
    expect_is(x, "character")
})
